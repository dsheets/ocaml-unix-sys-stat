(*
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
 *)

include Unix_sys_stat_common

open Ctypes
open Foreign
open PosixTypes
open Unsigned

let of_dev_t     = coerce dev_t     uint64_t
let of_ino_t     = coerce ino_t     uint64_t
let of_mode_t    = coerce mode_t    uint32_t
let of_nlink_t   = coerce nlink_t   uint64_t
let of_uid_t     = coerce uid_t     uint32_t
let of_gid_t     = coerce gid_t     uint32_t
let of_off_t     = coerce off_t     int64_t
let of_time_t    = coerce time_t    int64_t

let float_of_time_t t = Int64.to_float (of_time_t t)

module Stat = struct
  type t
  let t : t structure typ = structure "Stat"
  let ( -:* ) s x = field t s x
  let dev     = "dev"       -:* dev_t
  let ino     = "ino"       -:* ino_t
  let nlink   = "nlink"     -:* nlink_t
  let mode    = "mode"      -:* mode_t
  let uid     = "uid"       -:* uid_t
  let gid     = "gid"       -:* gid_t
  let _       = "pad0"      -:* uint (* wtf *)
  let rdev    = "rdev"      -:* dev_t
  let size    = "size"      -:* off_t
  let blksize = "blksize"   -:* blksize_t
  let blocks  = "blocks"    -:* blkcnt_t
  let atime   = "atime"     -:* time_t
  let _       = "atimensec" -:* ulong
  let mtime   = "mtime"     -:* time_t
  let _       = "mtimensev" -:* ulong
  let ctime   = "ctime"     -:* time_t
  let _       = "ctimensec" -:* ulong

  let () = seal t

  let to_unix t = Ctypes.(Unix.LargeFile.({
    st_dev   = UInt64.to_int (of_dev_t (getf t dev)); (* TODO: major/minor? *)
    st_ino   = UInt64.to_int (of_ino_t (getf t ino));
    st_kind  = File_kind.(of_code_exn ~host
                            (UInt32.to_int (of_mode_t (getf t mode))));
    st_perm  = File_perm.(full_of_code ~host
                            (UInt32.to_int (of_mode_t (getf t mode))));
    st_nlink = UInt64.to_int (of_nlink_t (getf t nlink));
    st_uid   = UInt32.to_int (of_uid_t (getf t uid));
    st_gid   = UInt32.to_int (of_gid_t (getf t gid));
    st_rdev  = UInt64.to_int (of_dev_t (getf t rdev)); (* TODO: major/minor? *)
    st_size  = of_off_t (getf t size);
    st_atime = float_of_time_t (getf t atime);
    st_mtime = float_of_time_t (getf t mtime);
    st_ctime = float_of_time_t (getf t ctime);
  }))
end

external unix_sys_stat_sizeof_stat
  : unit -> int = "unix_sys_stat_sizeof_stat" "noalloc"

let sizeof_stat = unix_sys_stat_sizeof_stat ()
let make_stat () = !@ (
  coerce (ptr uint8_t) (ptr Stat.t) (allocate_n uint8_t ~count:sizeof_stat)
)

let local ?check_errno addr typ =
  coerce (ptr void) (funptr ?check_errno typ) (ptr_of_raw_address addr)

external unix_sys_stat_mknod_ptr : unit -> int64 = "unix_sys_stat_mknod_ptr"

let mknod =
  let c = local ~check_errno:true (unix_sys_stat_mknod_ptr ())
    PosixTypes.(string @-> mode_t @-> dev_t @-> returning int)
  in
  fun pathname mode dev ->
    try ignore (c pathname mode dev)
    with Unix.Unix_error(e,_,_) -> raise (Unix.Unix_error (e,"mknod",pathname))

external unix_sys_stat_stat_ptr : unit -> int64 = "unix_sys_stat_stat_ptr"

let stat =
  let c = local ~check_errno:true (unix_sys_stat_stat_ptr ())
    PosixTypes.(string @-> ptr Stat.t @-> returning int)
  in
  fun path ->
    let stat = make_stat () in
    try (ignore (c path (addr stat)); stat)
    with Unix.Unix_error(e,_,_) -> raise (Unix.Unix_error (e,"stat",path))

external unix_sys_stat_lstat_ptr : unit -> int64 = "unix_sys_stat_lstat_ptr"

let lstat =
  let c = local ~check_errno:true (unix_sys_stat_lstat_ptr ())
    PosixTypes.(string @-> ptr Stat.t @-> returning int)
  in
  fun path ->
    let stat = make_stat () in
    try (ignore (c path (addr stat)); stat)
    with Unix.Unix_error(e,_,_) -> raise (Unix.Unix_error (e,"lstat",path))

external unix_sys_stat_fstat_ptr : unit -> int64 = "unix_sys_stat_fstat_ptr"

let fstat =
  let c = local ~check_errno:true (unix_sys_stat_fstat_ptr ())
    PosixTypes.(int @-> ptr Stat.t @-> returning int)
  in
  fun fd ->
    let stat = make_stat () in
    try (ignore (c (Fd_send_recv.int_of_fd fd) (addr stat)); stat)
    with Unix.Unix_error(e,_,_) -> raise (Unix.Unix_error (e,"fstat",""))
