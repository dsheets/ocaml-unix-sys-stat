/*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#define _BSD_SOURCE

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/threads.h>

#define IGN(x) (void)(x)
#define v value

#ifndef S_IFMT
#define S_IFMT (-1)
#endif
#ifndef S_IFDIR
#define S_IFDIR (-1)
#endif
#ifndef S_IFCHR
#define S_IFCHR (-1)
#endif
#ifndef S_IFBLK
#define S_IFBLK (-1)
#endif
#ifndef S_IFREG
#define S_IFREG (-1)
#endif
#ifndef S_IFIFO
#define S_IFIFO (-1)
#endif
#ifndef S_IFLNK
#define S_IFLNK (-1)
#endif
#ifndef S_IFSOCK
#define S_IFSOCK (-1)
#endif

CAMLprim v unix_sys_stat_s_ifmt(v _) { IGN(_); return Val_int(S_IFMT); }
CAMLprim v unix_sys_stat_s_ifdir(v _) { IGN(_); return Val_int(S_IFDIR); }
CAMLprim v unix_sys_stat_s_ifchr(v _) { IGN(_); return Val_int(S_IFCHR); }
CAMLprim v unix_sys_stat_s_ifblk(v _) { IGN(_); return Val_int(S_IFBLK); }
CAMLprim v unix_sys_stat_s_ifreg(v _) { IGN(_); return Val_int(S_IFREG); }
CAMLprim v unix_sys_stat_s_ififo(v _) { IGN(_); return Val_int(S_IFIFO); }
CAMLprim v unix_sys_stat_s_iflnk(v _) { IGN(_); return Val_int(S_IFLNK); }
CAMLprim v unix_sys_stat_s_ifsock(v _) { IGN(_); return Val_int(S_IFSOCK); }

CAMLprim v unix_sys_stat_s_irwxu(v _) { IGN(_); return Val_int(S_IRWXU); }
CAMLprim v unix_sys_stat_s_irwxg(v _) { IGN(_); return Val_int(S_IRWXG); }
CAMLprim v unix_sys_stat_s_irwxo(v _) { IGN(_); return Val_int(S_IRWXO); }
CAMLprim v unix_sys_stat_s_isuid(v _) { IGN(_); return Val_int(S_ISUID); }
CAMLprim v unix_sys_stat_s_isgid(v _) { IGN(_); return Val_int(S_ISGID); }
CAMLprim v unix_sys_stat_s_isvtx(v _) { IGN(_); return Val_int(S_ISVTX); }

CAMLprim v unix_sys_stat_sizeof_stat(v _) {
  IGN(_); return Val_int(sizeof(struct stat));
}

int unix_sys_stat_mkdir(const char *pathname, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = mkdir(pathname, mode);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_mkdir_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_mkdir);
}

int unix_sys_stat_mknod(const char *pathname, mode_t mode, dev_t dev) {
  int retval;
  caml_release_runtime_system();
  retval = mknod(pathname, mode, dev);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_mknod_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_mknod);
}

int unix_sys_stat_stat(const char *path, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = stat(path, buf);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_stat_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_stat);
}

int unix_sys_stat_lstat(const char *path, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = lstat(path, buf);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_lstat_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_lstat);
}

int unix_sys_stat_fstat(int fd, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = fstat(fd, buf);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_fstat_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_fstat);
}

int unix_sys_stat_chmod(const char *path, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = chmod(path, mode);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_chmod_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_chmod);
}

int unix_sys_stat_fchmod(int fd, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = fchmod(fd, mode);
  caml_acquire_runtime_system();
  return retval;
}

v unix_sys_stat_fchmod_ptr (v _) {
  IGN(_); return caml_copy_int64((intptr_t)(void *)unix_sys_stat_fchmod);
}
