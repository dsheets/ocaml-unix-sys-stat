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

int unix_sys_stat_mkdir(const char *pathname, mode_t mode);

int unix_sys_stat_mknod(const char *pathname, mode_t mode, dev_t dev);

int unix_sys_stat_stat(const char *path, struct stat *buf);

int unix_sys_stat_lstat(const char *path, struct stat *buf);

int unix_sys_stat_fstat(int fd, struct stat *buf);

int unix_sys_stat_chmod(const char *path, mode_t mode);

int unix_sys_stat_fchmod(int fd, mode_t mode);

int unix_sys_stat_fstatat(int dirfd, const char *pathname, struct stat *buf,
                          int flags);
