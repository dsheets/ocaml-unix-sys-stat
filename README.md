ocaml-unix-sys-stat
===================

[ocaml-unix-sys-stat](https://github.com/dsheets/ocaml-unix-sys-stat) provides
access to the features exposed in [`sys/stat.h`][sys_stat.h] in a way that is not
tied to the implementation on the host system.

The [`Sys_stat`][sys_stat] module provides functions for translating between
the file types and mode bits accessible through `sys/stat.h` and their values
on particular systems.  The [`Sys_stat_host`][sys_stat_host] module exports
representations of various hosts.

The [`Sys_stat_unix`][sys_stat_unix] provides bindings to functions that use the
types in `Sys_stat` along with a representation of the host system.  The bindings
support a more comprehensive range of flags than the corresponding functions
in the standard OCaml `Unix` module.

[sys_stat.h]: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_stat.h.html
[sys_stat]: https://github.com/dsheets/ocaml-unix-sys-stat/blob/master/lib/sys_stat.mli
[sys_stat_host]: https://github.com/dsheets/ocaml-unix-sys-stat/blob/master/lib/sys_stat_host.mli
[sys_stat_unix]: https://github.com/dsheets/ocaml-unix-sys-stat/blob/master/unix/sys_stat_unix.mli
[lwt]: http://ocsigen.org/lwt/
