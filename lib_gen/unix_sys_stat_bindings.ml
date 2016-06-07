(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
 *)

open Ctypes
open Posix_types

module Types = Unix_sys_stat_types.C(Unix_sys_stat_types_detected)

module C(F: Cstubs.FOREIGN) = struct

  let mkdir = F.(foreign "mkdir" (
    string @-> mode_t @-> returning int
  ))

  let mknod = F.(foreign "mknod" (
    string @-> mode_t @-> dev_t @-> returning int
  ))

  let stat = F.(foreign "stat" (
    string @-> ptr Types.Stat.t @-> returning int
  ))

  let lstat = F.(foreign "lstat" (
    string @-> ptr Types.Stat.t @-> returning int
  ))

  let fstat = F.(foreign "fstat" (
    int @-> ptr Types.Stat.t @-> returning int
  ))

  let chmod = F.(foreign "chmod" (
    string @-> mode_t @-> returning int
  ))

  let fchmod = F.(foreign "fchmod" (
    int @-> mode_t @-> returning int
  ))

  let fstatat = F.(foreign "fstatat" (
      int @-> string @-> ptr Types.Stat.t @-> int @-> returning int
  ))
end
