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

module Stat : sig
  open Posix_types

  type t

  val dev    : t -> dev_t
  val ino    : t -> ino_t
  val nlink  : t -> nlink_t
  val mode   : t -> mode_t
  val uid    : t -> uid_t
  val gid    : t -> gid_t
  val rdev   : t -> dev_t
  val size   : t -> off_t
  val blocks : t -> blkcnt_t
  val atime  : t -> time_t
  val mtime  : t -> time_t
  val ctime  : t -> time_t
  val to_unix : host:Sys_stat.Host.t -> t -> Unix.LargeFile.stats
end

val mkdir : string -> Sys_stat.Mode.t -> unit

val mknod : string -> Sys_stat.Mode.t -> dev:int -> unit

val stat : string -> Stat.t

(*
val lstat : string -> Stat.t

val fstat : Unix.file_descr -> Stat.t

val chmod : string -> Sys_stat.Mode.t -> unit

val fchmod : Unix.file_descr -> Sys_stat.Mode.t -> unit
*)
