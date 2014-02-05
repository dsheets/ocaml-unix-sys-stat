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

include module type of Unix_sys_stat_common

open Unsigned

(** TODO: field accessors *)
module Stat : sig
  type t

  val t : t Ctypes.structure Ctypes.typ

  val dev_int       : t Ctypes.structure -> uint64
  val ino_int       : t Ctypes.structure -> uint64
  val nlink_int     : t Ctypes.structure -> uint64
  val mode_int      : t Ctypes.structure -> uint32
  val uid_int       : t Ctypes.structure -> uint32
  val gid_int       : t Ctypes.structure -> uint32
  val rdev_int      : t Ctypes.structure -> uint64
  val size_int      : t Ctypes.structure -> int64
  val blocks_int    : t Ctypes.structure -> int64
  val atime_int     : t Ctypes.structure -> int64
  val atimensec_int : t Ctypes.structure -> uint32
  val mtime_int     : t Ctypes.structure -> int64
  val mtimensec_int : t Ctypes.structure -> uint32
  val ctime_int     : t Ctypes.structure -> int64
  val ctimensec_int : t Ctypes.structure -> uint32

  val to_unix : t Ctypes.structure -> Unix.LargeFile.stats
end

(** Can raise Unix.Unix_error *)
val mknod : string -> PosixTypes.mode_t -> PosixTypes.dev_t -> unit

(** Can raise Unix.Unix_error *)
val stat : string -> Stat.t Ctypes.structure

(** Can raise Unix.Unix_error *)
val lstat : string -> Stat.t Ctypes.structure

(** Can raise Unix.Unix_error *)
val fstat : Unix.file_descr -> Stat.t Ctypes.structure

(** Can raise Unix.Unix_error *)
val chmod : string -> PosixTypes.mode_t -> unit

(** Can raise Unix.Unix_error *)
val fchmod : Unix.file_descr -> PosixTypes.mode_t -> unit
