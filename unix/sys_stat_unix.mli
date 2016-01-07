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

module File_kind : sig
  val host : Sys_stat.File_kind.Host.t
  val to_unix : Sys_stat.File_kind.t -> Unix.file_kind
  val of_unix : Unix.file_kind -> Sys_stat.File_kind.t
end

module File_perm : sig
  val host : Sys_stat.File_perm.Host.t
end

module Mode : sig
  val host : Sys_stat.Mode.Host.t
end

val host : Sys_stat.Host.t

(*
module Stat : sig
  type t

  val of_dev_t : PosixTypes.dev_t -> int32
  val of_ino_t : PosixTypes.ino_t -> Unsigned.uint64
  val of_nlink_t : PosixTypes.nlink_t -> Unsigned.uint64
  val of_mode_t : PosixTypes.mode_t -> Unsigned.uint32
  val of_uid_t : Unsigned.uint64 -> Unsigned.uint64
  val of_gid_t : Unsigned.uint64 -> Unsigned.uint64
  val of_off_t : PosixTypes.off_t -> int64
  val of_blkcnt_t : Unsigned.uint64 -> int64
  val of_time_t : PosixTypes.time_t -> int64

  val dev_int : t -> int32
  val ino_int : t -> Unsigned.uint64
  val nlink_int : t -> Unsigned.uint64
  val mode_int : t -> Unsigned.uint32
  val uid_int : t -> Unsigned.uint64
  val gid_int : t -> Unsigned.uint64
  val rdev_int : t -> int32
  val size_int : t -> int64
  val blocks_int : t -> int64
  val atime_int : t -> int64
  val mtime_int : t -> int64
  val ctime_int : t -> int64
  val to_unix : host:Sys_stat.Host.t -> t -> Unix.LargeFile.stats
end

val mkdir : string -> Sys_stat.Mode.t -> unit

val mknod : string -> Sys_stat.Mode.t -> dev:int64 -> unit

val stat : string -> Stat.t

val lstat : string -> Stat.t

val fstat : Unix.file_descr -> Stat.t

val chmod : string -> Sys_stat.Mode.t -> unit

val fchmod : Unix.file_descr -> Sys_stat.Mode.t -> unit
*)
