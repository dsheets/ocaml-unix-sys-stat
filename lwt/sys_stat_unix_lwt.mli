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

open Sys_stat_unix

val lstat : string -> Stat.t Lwt.t

val stat : string -> Stat.t Lwt.t

val fstat : Unix.file_descr -> Stat.t Lwt.t

val mkdir : string -> Sys_stat.Mode.t -> unit Lwt.t

val mknod : string -> Sys_stat.Mode.t -> dev:int -> unit Lwt.t

val chmod : string -> Sys_stat.Mode.t -> unit Lwt.t

val fchmod : Unix.file_descr -> Sys_stat.Mode.t -> unit Lwt.t
