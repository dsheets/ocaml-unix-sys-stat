(*
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
 *)

module Type = Unix_sys_stat_types.C(Unix_sys_stat_types_detected)
module C = Unix_sys_stat_bindings.C(Unix_sys_stat_generated)

module File_kind = struct
  open Sys_stat.File_kind

  let host =
    let defns = Type.File_kind.({
      mask   = s_ifmt;
      dir    = s_ifdir;
      chr    = s_ifchr;
      blk    = s_ifblk;
      reg    = s_ifreg;
      fifo   = s_ififo;
      lnk    = s_iflnk;
      sock   = s_ifsock;
    }) in
    Host.of_defns defns

  let to_unix = Unix.(function
    | DIR  -> S_DIR
    | CHR  -> S_CHR
    | BLK  -> S_BLK
    | REG  -> S_REG
    | FIFO -> S_FIFO
    | LNK  -> S_LNK
    | SOCK -> S_SOCK
  )

  let of_unix = Unix.(function
    | S_DIR  -> DIR
    | S_CHR  -> CHR
    | S_BLK  -> BLK
    | S_REG  -> REG
    | S_FIFO -> FIFO
    | S_LNK  -> LNK
    | S_SOCK -> SOCK
  )
end

module File_perm = struct
  open Sys_stat.File_perm

  let host =
    let open Type.File_perm in
    let rwxu = s_irwxu in
    let rwxg = s_irwxg in
    let rwxo = s_irwxo in
    let access_mask = rwxu lor rwxg lor rwxo in
    let suid = s_isuid in
    let sgid = s_isgid in
    let svtx = s_isvtx in
    let full_mask = access_mask lor suid lor sgid lor svtx in
    let defns = {
      access_mask;
      full_mask;
      rwxu;
      rwxg;
      rwxo;
      suid;
      sgid;
      svtx;
    } in
    Host.of_defns defns

end

module Mode = struct
  let host = Sys_stat.Mode.Host.({
    file_kind = File_kind.host;
    file_perm = File_perm.host;
  })
end

let host = Sys_stat.Host.({
  file_kind = File_kind.host;
  file_perm = File_perm.host;
  mode = Mode.host;
})

(*
module Stat = struct
  open Ctypes
  open PosixTypes
  open Unsigned
  open Type.Stat

  type t = Type.Stat.t structure

  let of_dev_t     = coerce dev_t     int32_t
  let of_ino_t     = coerce ino_t     uint64_t
  let of_nlink_t   = coerce nlink_t   uint64_t
  let of_mode_t    = coerce mode_t    uint32_t
  let of_uid_t     = coerce uid_t     uint64_t
  let of_gid_t     = coerce gid_t     uint64_t
  let of_off_t     = coerce off_t     int64_t
  let of_blkcnt_t  = coerce blkcnt_t  int64_t
  let of_time_t    = coerce time_t    int64_t

  let dev_int s       = of_dev_t     (getf s st_dev)
  let ino_int s       = of_ino_t     (getf s st_ino)
  let nlink_int s     = of_nlink_t   (getf s st_nlink)
  let mode_int s      = of_mode_t    (getf s st_mode)
  let uid_int s       = of_uid_t     (getf s st_uid)
  let gid_int s       = of_gid_t     (getf s st_gid)
  let rdev_int s      = of_dev_t     (getf s st_rdev)
  let size_int s      = of_off_t     (getf s st_size)
  let blocks_int s    = of_blkcnt_t  (getf s st_blocks)
  let atime_int s     = of_time_t    (getf s st_atime)
  let mtime_int s     = of_time_t    (getf s st_mtime)
  let ctime_int s     = of_time_t    (getf s st_ctime)

  let to_unix ~host t =
    let (st_kind, st_perm) = Sys_stat.Mode.(
      of_code_exn ~host:Mode.host (UInt32.to_int (mode_int t))
    ) in
    Ctypes.(Unix.LargeFile.({
      st_dev   = Int32.to_int (dev_int t);
      st_ino   = UInt64.to_int (ino_int t);
      st_kind  = File_kind.to_unix st_kind;
      st_perm;
      st_nlink = UInt64.to_int (nlink_int t);
      st_uid   = UInt64.to_int (uid_int t);
      st_gid   = UInt64.to_int (gid_int t);
      st_rdev  = Int32.to_int (rdev_int t);
      st_size  = size_int t;
      st_atime = Int64.to_float (atime_int t);
      st_mtime = Int64.to_float (mtime_int t);
      st_ctime = Int64.to_float (ctime_int t);
    }))

end

let mkdir name mode =
  Errno_unix.raise_on_errno ~call:"mkdir" ~label:name (fun () ->
    (*let mode = Int32.of_int (Sys_stat.Mode.to_code ~host:Mode.host mode) in*)
    let mode = Ctypes.(coerce uint32_t PosixTypes.mode_t Unsigned.UInt32.zero) in
    ignore (C.mkdir name mode)
  )
*)
    

let mknod name mode ~dev =
  Errno_unix.raise_on_errno ~call:"mknod" ~label:name (fun () ->
    let dev = PosixTypes.Dev.of_int dev in
    let mode = PosixTypes.Mode.of_int (Sys_stat.Mode.to_code ~host:Mode.host mode) in
    if C.mknod name mode dev <> 0
    then None
    else Some ()
  )

(*
let stat name =
  Errno_unix.raise_on_errno ~call:"stat" ~label:name (fun () ->
    let stat = Ctypes.make Type.Stat.t in
    ignore (C.stat name (Ctypes.addr stat));
    stat
  )

let lstat name =
  Errno_unix.raise_on_errno ~call:"lstat" ~label:name (fun () ->
    let stat = Ctypes.make Type.Stat.t in
    ignore (C.lstat name (Ctypes.addr stat));
    stat
  )

let fstat fd =
  Errno_unix.raise_on_errno ~call:"fstat" (fun () ->
    let stat = Ctypes.make Type.Stat.t in
    ignore (C.fstat (Fd_send_recv.int_of_fd fd) (Ctypes.addr stat));
    stat
  )

let chmod name mode =
  Errno_unix.raise_on_errno ~call:"chmod" ~label:name (fun () ->
    (*let mode = Int32.of_int (Sys_stat.Mode.to_code ~host:Mode.host mode) in*)
    let mode = Ctypes.(coerce uint32_t PosixTypes.mode_t Unsigned.UInt32.zero) in
    ignore (C.chmod name mode)
  )

let fchmod fd mode =
  Errno_unix.raise_on_errno ~call:"fchmod" (fun () ->
    (*let mode = Int32.of_int (Sys_stat.Mode.to_code ~host:Mode.host mode) in*)
    let mode = Ctypes.(coerce uint32_t PosixTypes.mode_t Unsigned.UInt32.zero) in
    ignore (C.fchmod (Fd_send_recv.int_of_fd fd) mode)
  )
*)
