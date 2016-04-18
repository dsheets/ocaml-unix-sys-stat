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
  type t = DIR | CHR | BLK | REG | FIFO | LNK | SOCK

  type defns = {
    mask : int;
    dir : int;
    chr : int;
    blk : int;
    reg : int;
    fifo : int;
    lnk : int;
    sock : int;
  }

  module Host : sig
    type t

    val of_defns : defns -> t
    val to_defns : t -> defns

    val to_string : t -> string
    val of_string : string -> t
  end

  val to_char : t -> char
  val to_string : t -> string
  val to_code : host:Host.t -> t -> int
  val of_code_exn : host:Host.t -> int -> t
  val of_code : host:Host.t -> int -> t option
end

module File_perm : sig
  type t = int

  type defns = {
    access_mask : int;
    full_mask : int;
    rwxu : int;
    rwxg : int;
    rwxo : int;
    suid : int;
    sgid : int;
    svtx : int;
  }

  module Host : sig
    type t

    val of_defns : defns -> t
    val to_defns : t -> defns

    val to_string : t -> string
    val of_string : string -> t
  end

  val access_of_code : host:Host.t -> int -> t
  val full_of_code   : host:Host.t -> int -> t
  val to_code        : host:Host.t -> t   -> int

  val is_suid   : host:Host.t -> t -> bool
  val is_sgid   : host:Host.t -> t -> bool
  val is_sticky : host:Host.t -> t -> bool

  val to_string : host:Host.t -> t      -> string
  val of_string : host:Host.t -> string -> t
end

module Mode : sig
  type t = File_kind.t * File_perm.t

  module Host : sig
    type t = {
      file_kind : File_kind.Host.t;
      file_perm : File_perm.Host.t;
    }

    val to_string : t -> string
    val of_string : string -> t
  end

  val to_string   : host:Host.t -> t      -> string
  val of_string   : host:Host.t -> string -> t

  val to_code     : host:Host.t -> t      -> int
  val of_code_exn : host:Host.t -> int    -> t
  val of_code     : host:Host.t -> int    -> t option
end

module Host : sig
  type t = {
    file_kind : File_kind.Host.t;
    file_perm : File_perm.Host.t;
    mode      : Mode.Host.t;
  }

  val to_string : t -> string
  val of_string : string -> t
end
