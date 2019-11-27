open Rresult

let search directories library =
  let rec go = function
    | [] -> R.error_msgf "Library <%a> does not exist." Fpath.pp library
    | x :: r ->
      let path = Fpath.(x // library) in
      Bos.OS.File.exists path >>= function
      | true -> Ok path
      | false -> go r in
  go directories

let with_ccopt_L ppf path =
  Fmt.pf ppf "-ccopt@\n-L%a" Fpath.pp path

let with_cclib_l ppf = function
  | `Name name -> Fmt.pf ppf "-cclib@\n-l%s" name
  | `Filename path -> Fmt.pf ppf "-cclib@\n-l:%a" Fpath.pp path

let with_ccopt ppf x =
  Fmt.pf ppf "-ccopt@\n%s" x

let resolve directories libraries =
  let rec go resolved = function
    | [] -> R.ok (List.rev resolved)
    | `Name library :: r ->
      let path = Fpath.(v ("lib" ^ library) + "a") in
      search directories path >>= fun x -> go (x :: resolved) r
    | `Filename path :: r ->
      if Fpath.is_rel path
      then search directories path >>= fun x -> go (x :: resolved) r
      else Bos.OS.File.exists path >>= function
        | true -> go (path :: resolved) r
        | false -> R.error_msgf "Library <%a> does not exist." Fpath.pp path in
  go [] libraries >>= fun _ ->
  Ok (directories, libraries)

let is_opt s = String.length s > 1 && s.[0] = '-'
let is_short_opt s = String.length s = 2 && s.[0] = '-'

let parse_opt_arg s =
  let l = String.length s in
  if s.[1] <> '-'
  then
    if l = 2 then s, None
    else String.sub s 0 2, Some (String.sub s 2 (l - 2))
  else
    try
      let i = String.index s '=' in
      String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1))
    with Not_found -> s, None

let parse_lL_args args =
  let rec go lL_args other_args = function
    | [] -> R.ok (List.rev lL_args, List.rev other_args)
    | "--" :: args -> R.ok (List.rev lL_args, List.rev_append other_args args)
    | x :: args ->
      if not (is_opt x)
      then go lL_args (x :: other_args) args
      else
        let name, value = parse_opt_arg x in
        match name with
        | "-L" | "--library-directory" | "--library-path" | "-l" | "--library" ->
          ( match value with
            | Some value -> go ((name, value) :: lL_args) other_args args
            | None -> match args with
              | [] -> R.error_msgf "%s must have a value." name
              | value :: args ->
                if is_opt value
                then R.error_msgf "%s must have a value." name
                else go ((name, value) :: lL_args) other_args args )
        | _ -> go lL_args (x :: other_args) args in
  go [] [] args

let to_cmdliner ~binary lL_args =
  let res = Array.make (1 + List.length lL_args) "" in
  res.(0) <- binary ;
  List.iteri (fun i (k, v) -> res.(i + 1) <- Fmt.strf "%s%s" k v) lL_args ;
  res

let resolve directories libraries =
  resolve directories libraries |> function
  | Ok v -> `Ok (Ok v)
  | Error (`Msg err) -> `Ok (Error (`Msg err))

open Cmdliner

let existing_directory =
  let parser x = match Fpath.of_string x with
    | Ok v when Fpath.is_dir_path v && Sys.is_directory x -> Ok v
    | Ok v when Sys.is_directory x -> Ok (Fpath.to_dir_path v)
    | Ok v -> R.error_msgf "Directory <%a> does not exist" Fpath.pp v
    | Error err -> Error err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<directory>" (parser, pp)

type namespec =
  [ `Filename of Fpath.t
  | `Name of string ]

let library =
  let parser x = match Astring.String.cut ~sep:":" x with
    | Some ("", path) ->
      ( match Fpath.of_string path with
        | Ok v when Fpath.is_abs v && Sys.file_exists path -> Ok (`Filename v)
        | Ok v when Fpath.is_rel v -> Ok (`Filename v)
        | Ok v -> R.error_msgf "Library <%a> does not exist" Fpath.pp v
        | Error err -> Error err )
    | Some (_, _) -> R.error_msgf "Invalid <namespec> %S" x
    | None ->
      ( match Fpath.of_string x with
        | Ok v when Fpath.is_file_path v && Fpath.filename v = x -> Ok (`Name x)
        | Ok v -> R.error_msgf "Invalid library name <%a>" Fpath.pp v
        | Error err -> Error err) in
  let pp ppf = function
    | `Filename path -> Fmt.pf ppf ":%a" Fpath.pp path
    | `Name x -> Fmt.string ppf x in
  Arg.conv ~docv:"<library>" (parser, pp)

let path_of_libraries =
  let doc = "Add directory $(i,dir) to the list of directoryies to be searched for $(b,-l)." in
  let docv = "dir" in
  Arg.(value & opt_all existing_directory [] & info [ "L"; "library-directory"; "library-path" ] ~doc ~docv)

let libraries =
  let doc = "Add the archive or object file specified by $(i,namespec) to the list of files to link. \
             This option may be used any number of times. \
             If $(i,namespec) is of the form $(i,:filename), $(b,ld) wil lsearch the library path for a file called $(i,filename), \
             otherwise it will search the library path for a file called $(i,libnamespec.a)." in
  let docv = "namespec" in
  Arg.(value & opt_all library [] & info [ "l"; "library" ] ~doc ~docv)

let cmd =
  let doc = "Ocamloptocc" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Transform a $(i,ld)/$(i,cc) command-line to an $(i,ocamlopt) command-line." ] in
  Term.(ret (const resolve $ path_of_libraries $ libraries)),
  Term.info "ocamloptocc" ~doc ~exits ~man

let null_buffer = Buffer.create 0x100
let null = Format.formatter_of_buffer null_buffer

let process_argv argv =
  parse_lL_args (List.tl (Array.to_list argv)) >>= fun (lL_args, other_args) ->
  let argv = to_cmdliner ~binary:"a.out" lL_args in
  ( match Cmdliner.Term.eval ~help:null ~err:null ~catch:false ~argv cmd with
    | `Error `Exn -> R.error `Exn
    | `Error `Term -> R.error `Term
    | `Error `Parse -> R.error `Parse
    | `Ok (Ok v) -> Ok (`Do (v, other_args))
    | `Ok (Error (`Msg err)) -> R.error_msg err
    | `Version -> Ok `Version
    | `Help -> Ok `Help )

let run argv =
  process_argv argv |> function
  | Ok (`Do ((directories, libraries), other_args)) ->
    if List.length directories > 0 then Fmt.pr "%a@\n" Fmt.(list ~sep:(always "@\n") with_ccopt_L) directories ;
    if List.length libraries > 0 then Fmt.pr "%a@\n" Fmt.(list ~sep:(always "@\n") with_cclib_l) libraries ;
    if List.length other_args > 0 then Fmt.pr "%a@\n" Fmt.(list ~sep:(always "@\n") with_ccopt) other_args ;
    Ok ()
  | Ok `Version -> Ok () (* XXX(dinosaure): do nothing. *)
  | Ok `Help -> Ok () (* XXX(dinosaure): do nothing. *)
  | Error (`Exn | `Term | `Parse as err) -> Error (`Cmd err)
  | Error (`Msg err) -> Error (`Msg err)

let () = match run Sys.argv with
  | Ok () -> ()
  | Error (`Msg err) -> Fmt.epr "[%a] %s\n%!" Fmt.(styled `Red string) "ERROR" err
  | Error _ -> ()
