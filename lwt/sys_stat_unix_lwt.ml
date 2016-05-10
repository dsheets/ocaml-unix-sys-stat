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

type lstat_result =
  | Stat_info of Sys_stat_unix.Stat.t
  | Stat_error of int

external lstat_job :
  string -> Sys_stat_unix.Stat.t -> nativeint -> lstat_result Lwt_unix.job =
  "unix_sys_stat_lwt_lstat_job"

let lstat path =
  let stat = Ctypes.make Sys_stat_unix.Stat.t in
  let job = lstat_job path stat Ctypes.(raw_address_of_ptr (to_voidp (addr stat))) in
  let open Lwt in 
  Lwt_unix.run_job job >>= function
  | Stat_info stat ->
    Lwt.return stat
  | Stat_error errno -> 
    Errno_unix.raise_errno ~call:"lstat" ~label:path errno
