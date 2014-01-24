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

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

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

CAMLprim value unix_sys_stat_s_ifmt() { return Val_int(S_IFMT); }
CAMLprim value unix_sys_stat_s_ifdir() { return Val_int(S_IFDIR); }
CAMLprim value unix_sys_stat_s_ifchr() { return Val_int(S_IFCHR); }
CAMLprim value unix_sys_stat_s_ifblk() { return Val_int(S_IFBLK); }
CAMLprim value unix_sys_stat_s_ifreg() { return Val_int(S_IFREG); }
CAMLprim value unix_sys_stat_s_ififo() { return Val_int(S_IFIFO); }
CAMLprim value unix_sys_stat_s_iflnk() { return Val_int(S_IFLNK); }
CAMLprim value unix_sys_stat_s_ifsock() { return Val_int(S_IFSOCK); }

value unix_sys_stat_mknod_ptr (value _) {
  return caml_copy_int64((intptr_t)(void *)mknod);
}
