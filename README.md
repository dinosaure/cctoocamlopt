## cc to ocamlopt

The goal of this little project is to check static libraries given by a piece of
CC flags (by `pkg-config` or something else). Then, it wraps any `-L` flags with
`-ccopt` and `-l` flags with `-cclib`. Another check is that static libraries
exist in your computer (this tool does not expand any variables like `OPAM` or
`PKG\_CONFIG`).

Then, it re-order `-L` and `-l` flags. Indeed, `-l` can find libraries by the
order of given `-L` (and environment variable like `LD_LIBRARY_PATH`). So be
more predictable about what is statically linked in your binary, we move all
`-L` in front of your command-line and tell you which libraries we will choose
(the first founded as expected by GCC linker).

TODO:
* handle `LD_LIBRARY_PATH`
* tests
