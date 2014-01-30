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

module File_kind = struct
  type t = Unix.file_kind

  external s_ifmt   : unit -> int = "unix_sys_stat_s_ifmt"   "noalloc"
  external s_ifdir  : unit -> int = "unix_sys_stat_s_ifdir"  "noalloc"
  external s_ifchr  : unit -> int = "unix_sys_stat_s_ifchr"  "noalloc"
  external s_ifblk  : unit -> int = "unix_sys_stat_s_ifblk"  "noalloc"
  external s_ifreg  : unit -> int = "unix_sys_stat_s_ifreg"  "noalloc"
  external s_ififo  : unit -> int = "unix_sys_stat_s_ififo"  "noalloc"
  external s_iflnk  : unit -> int = "unix_sys_stat_s_iflnk"  "noalloc"
  external s_ifsock : unit -> int = "unix_sys_stat_s_ifsock" "noalloc"

  (* TODO: Are these optional? POSIX defines all but IFSOCK *)
  type defns = {
    s_ifmt   : int option;
    s_ifdir  : int option;
    s_ifchr  : int option;
    s_ifblk  : int option;
    s_ifreg  : int option;
    s_ififo  : int option;
    s_iflnk  : int option;
    s_ifsock : int option;
  }

  type index = (int, t) Hashtbl.t
  type host = defns * index

  let to_string = Unix.(function
    | S_REG   -> "S_REG"
    | S_DIR   -> "S_DIR"
    | S_CHR   -> "S_CHR"
    | S_BLK   -> "S_BLK"
    | S_LNK   -> "S_LNK"
    | S_FIFO  -> "S_FIFO"
    | S_SOCK  -> "S_SOCK"
  )

  let to_code ~host = let (defns,_) = host in Unix.(function
    | S_REG   -> defns.s_ifreg
    | S_DIR   -> defns.s_ifdir
    | S_CHR   -> defns.s_ifchr
    | S_BLK   -> defns.s_ifblk
    | S_LNK   -> defns.s_iflnk
    | S_FIFO  -> defns.s_ififo
    | S_SOCK  -> defns.s_ifsock
  )

  let index_of_defns defns =
    let open Unix in
    let open Hashtbl in
    let h = create 10 in
    (match defns.s_ifreg with
    | Some x -> replace h x S_REG | None -> ());
    (match defns.s_ifdir with
    | Some x -> replace h x S_DIR | None -> ());
    (match defns.s_ifchr with
    | Some x -> replace h x S_CHR | None -> ());
    (match defns.s_ifblk with
    | Some x -> replace h x S_BLK | None -> ());
    (match defns.s_iflnk with
    | Some x -> replace h x S_LNK | None -> ());
    (match defns.s_ififo with
    | Some x -> replace h x S_FIFO | None -> ());
    (match defns.s_ifsock with
    | Some x -> replace h x S_SOCK | None -> ());
    h

  let host =
    let option f = match f () with -1 -> None | x -> Some x in
    let defns = {
      s_ifmt   = option s_ifmt;
      s_ifdir  = option s_ifdir;
      s_ifchr  = option s_ifchr;
      s_ifblk  = option s_ifblk;
      s_ifreg  = option s_ifreg;
      s_ififo  = option s_ififo;
      s_iflnk  = option s_iflnk;
      s_ifsock = option s_ifsock;
    } in
    let index = index_of_defns defns in
    (defns,index)

  let of_code_exn ~host code =
    let (_,index) = host in
    Hashtbl.find index (code land (s_ifmt ()))

  let of_code ~host code =
    try Some (of_code_exn ~host code) with Not_found -> None
end

module File_perm = struct
  type t = Unix.file_perm

  external s_irwxu : unit -> int = "unix_sys_stat_s_irwxu" "noalloc"
  external s_irwxg : unit -> int = "unix_sys_stat_s_irwxg" "noalloc"
  external s_irwxo : unit -> int = "unix_sys_stat_s_irwxo" "noalloc"
  external s_isuid : unit -> int = "unix_sys_stat_s_isuid" "noalloc"
  external s_isgid : unit -> int = "unix_sys_stat_s_isgid" "noalloc"
  external s_isvtx : unit -> int = "unix_sys_stat_s_isvtx" "noalloc"

  type defns = {
    access_mask : int;
    full_mask   : int;
    s_isuid     : int;
    s_isgid     : int;
    s_isvtx     : int;
  }
  type host = defns

  let host =
    let access_mask = (s_irwxu ()) lor (s_irwxg ()) lor (s_irwxo ()) in
    let s_isuid = s_isuid () in
    let s_isgid = s_isgid () in
    let s_isvtx = s_isvtx () in
    let full_mask = access_mask lor s_isuid lor s_isgid lor s_isvtx in {
      access_mask;
      full_mask;
      s_isuid;
      s_isgid;
      s_isvtx;
    }

  let access_of_code ~host code = code land host.access_mask
  let full_of_code   ~host code = code land host.full_mask

  let is_suid   ~host code = (code land host.s_isuid) = host.s_isuid
  let is_sgid   ~host code = (code land host.s_isgid) = host.s_isgid
  let is_sticky ~host code = (code land host.s_isvtx) = host.s_isvtx
end

type host = {
  file_kind : File_kind.host;
  file_perm : File_perm.host;
}
let host = {
  file_kind = File_kind.host;
  file_perm = File_perm.host;
}
