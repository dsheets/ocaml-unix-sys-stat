(*
 * Copyright (c) 2016 Jeremy Yallop <yallop@docker.com>
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

module Generated = Unix_sys_stat_lwt_generated
module C = Unix_sys_stat_bindings.C(Generated)
open Lwt.Infix

let decode_mode mode =
  Posix_types.Mode.of_int (Sys_stat.Mode.to_code ~host:Sys_stat_unix.Mode.host mode)

let lstat path =
  let stat = Ctypes.make Sys_stat_unix.Stat.t in
  (C.lstat path (Ctypes.addr stat)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"lstat" ~label:path errno
  else Lwt.return stat

let stat path =
  let stat = Ctypes.make Sys_stat_unix.Stat.t in
  (C.stat path (Ctypes.addr stat)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"stat" ~label:path errno
  else Lwt.return stat

let fstat fd =
  let fd = Unix_representations.int_of_file_descr fd in
  let stat = Ctypes.make Sys_stat_unix.Stat.t in
  (C.fstat fd (Ctypes.addr stat)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"fstat" ~label:(string_of_int fd) errno
  else Lwt.return stat

let mkdir path mode =
  (C.mkdir path (decode_mode mode)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"mkdir" ~label:path errno
  else Lwt.return_unit

let mknod path mode ~dev =
  let dev = Posix_types.Dev.of_int dev in
  (C.mknod path (decode_mode mode) dev).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"mknod" ~label:path errno
  else Lwt.return_unit

let chmod path mode =
  (C.chmod path (decode_mode mode)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"chmod" ~label:path errno
  else Lwt.return_unit

let fchmod fd mode =
  let fd = Unix_representations.int_of_file_descr fd in
  (C.fchmod fd (decode_mode mode)).Generated.lwt >>= fun (i, errno) ->
  if i < 0 then Errno_unix.raise_errno ~call:"fchmod" ~label:(string_of_int fd) errno
  else Lwt.return_unit

let fstatat fd pathname ~flags =
  let flags =
    match flags with
    | Some (Sys_stat.At.Symlink_nofollow as nf) ->
      Sys_stat.At.to_code ~host:Sys_stat_unix.At.host nf
    | None -> 0
  in
  let stat = Ctypes.make Sys_stat_unix.Stat.t in
  let fd = Unix_representations.int_of_file_descr fd in
  (C.fstatat fd pathname (Ctypes.addr stat) flags).Generated.lwt >>= fun (i, errno) ->
  if i <> 0
  then Errno_unix.raise_errno ~call:"fstatat" ~label:pathname errno
  else Lwt.return stat

