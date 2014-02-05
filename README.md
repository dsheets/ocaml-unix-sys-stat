ocaml-unix-sys-stat
================

[ocaml-unix-sys-stat](https://github.com/dsheets/ocaml-unix-sys-stat) provides
host-dependent sys/stat.h access.

**WARNING**: not portable due to *mknod*, *stat*, *lstat*, *fstat*,
*chmod*, and *fchmod* wrappers that assume 64-bit instruction pointers.
Also, POSIX type coercions sizes are hard-coded and the *stat* struct layout is
specific to Linux x86-64. This is not ideal.
