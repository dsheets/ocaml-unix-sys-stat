/*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

#include <sys/stat.h>
#include <caml/alloc.h>
#include <caml/threads.h>

int unix_sys_stat_mkdir(const char *pathname, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = mkdir(pathname, mode);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_mknod(const char *pathname, mode_t mode, dev_t dev) {
  int retval;
  caml_release_runtime_system();
  retval = mknod(pathname, mode, dev);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_stat(const char *path, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = stat(path, buf);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_lstat(const char *path, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = lstat(path, buf);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_fstat(int fd, struct stat *buf) {
  int retval;
  caml_release_runtime_system();
  retval = fstat(fd, buf);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_chmod(const char *path, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = chmod(path, mode);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_fchmod(int fd, mode_t mode) {
  int retval;
  caml_release_runtime_system();
  retval = fchmod(fd, mode);
  caml_acquire_runtime_system();
  return retval;
}

int unix_sys_stat_fstatat(int dirfd, const char *pathname,
                          struct stat *buf, int flags) {
  int retval;
  caml_release_runtime_system();
  retval = fstatat(dirfd, pathname, buf, flags);
  caml_acquire_runtime_system();
  return retval;
}

