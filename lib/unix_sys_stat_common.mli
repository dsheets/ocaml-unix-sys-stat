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

type t = Unix.LargeFile.stats

module File_kind : sig
  type t = Unix.file_kind

  type host

  val host : host

  val to_code     : host:host -> t -> int
  val of_code_exn : host:host -> int -> t
  val of_code     : host:host -> int -> t option

  val to_string : t -> string
end

module File_perm : sig
  type t = Unix.file_perm

  type host

  val host : host

  val access_of_code : host:host -> int -> t
  val full_of_code   : host:host -> int -> t
  val to_code        : host:host -> t   -> int

  val is_suid   : host:host -> t -> bool
  val is_sgid   : host:host -> t -> bool
  val is_sticky : host:host -> t -> bool

  val to_string : host:host -> t -> string
end

module Mode : sig
  type t = File_kind.t * File_perm.t

  type host

  val host : host

  val to_string   : host:host -> t -> string
  val to_code     : host:host -> t -> int
  val of_code_exn : host:host -> int -> t
end

type host = {
  file_kind : File_kind.host;
  file_perm : File_perm.host;
  mode      : Mode.host;
}

val host : host
