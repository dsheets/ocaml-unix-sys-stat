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

(* Used for bit indexing.
   See <http://supertech.csail.mit.edu/papers/debruijn.pdf>. *)
let debruijn32 = 0x077CB531_l
let index32 = Array.make 32 0
let () = Array.(iteri (fun i _ ->
  let k = Int32.(to_int (shift_right_logical (shift_left debruijn32 i) 27)) in
  index32.(k) <- i
) index32)
let find_least_one i = index32.(
  Int32.(to_int (shift_right_logical (mul debruijn32 (logand i (neg i))) 27))
)

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

  type defns = {
    s_ifmt   : int;
    s_ifdir  : int;
    s_ifchr  : int;
    s_ifblk  : int;
    s_ifreg  : int;
    s_ififo  : int;
    s_iflnk  : int;
    s_ifsock : int;
  }

  type index = (int, t) Hashtbl.t
  type host = defns * index

  let to_char = Unix.(function
    | S_REG   -> '-'
    | S_DIR   -> 'd'
    | S_CHR   -> 'c'
    | S_BLK   -> 'b'
    | S_LNK   -> 'l'
    | S_FIFO  -> 'p'
    | S_SOCK  -> 's'
  )

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
    replace h defns.s_ifreg  S_REG;
    replace h defns.s_ifdir  S_DIR;
    replace h defns.s_ifchr  S_CHR;
    replace h defns.s_ifblk  S_BLK;
    replace h defns.s_iflnk  S_LNK;
    replace h defns.s_ififo  S_FIFO;
    replace h defns.s_ifsock S_SOCK;
    h

  let host =
    let check f name = match f () with
      | -1 -> raise (Failure ("<sys/stat.h> macro "^name^" missing"))
      | x -> x
    in
    let defns = {
      s_ifmt   = check s_ifmt   "S_IFMT";
      s_ifdir  = check s_ifdir  "S_IFDIR";
      s_ifchr  = check s_ifchr  "S_IFCHR";
      s_ifblk  = check s_ifblk  "S_IFBLK";
      s_ifreg  = check s_ifreg  "S_IFREG";
      s_ififo  = check s_ififo  "S_IFIFO";
      s_iflnk  = check s_iflnk  "S_IFLNK";
      s_ifsock = check s_ifsock "S_IFSOCK";
    } in
    let index = index_of_defns defns in
    (defns,index)

  let of_code_exn ~host code =
    let (defns,index) = host in
    Hashtbl.find index (code land defns.s_ifmt)

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

    s_irwxu     : int;
    s_irwxg     : int;
    s_irwxo     : int;

    s_isuid     : int;
    s_isgid     : int;
    s_isvtx     : int;
  }
  type host = defns

  let host =
    let s_irwxu = s_irwxu () in
    let s_irwxg = s_irwxg () in
    let s_irwxo = s_irwxo () in
    let access_mask = s_irwxu lor s_irwxg lor s_irwxo in
    let s_isuid = s_isuid () in
    let s_isgid = s_isgid () in
    let s_isvtx = s_isvtx () in
    let full_mask = access_mask lor s_isuid lor s_isgid lor s_isvtx in {
      access_mask;
      full_mask;
      s_irwxu;
      s_irwxg;
      s_irwxo;
      s_isuid;
      s_isgid;
      s_isvtx;
    }

  let mask_and_rshift mask code =
    (code land mask) lsr (find_least_one (Int32.of_int mask))

  let inject_and_lshift mask perm =
    perm lsl (find_least_one (Int32.of_int mask))

  (* TODO: these should standardize the bits *)
  let access_of_code ~host code = mask_and_rshift host.access_mask code
  let full_of_code   ~host code = mask_and_rshift host.full_mask   code
  let to_code        ~host perm = inject_and_lshift host.full_mask perm

  let is_suid   ~host code = (code land host.s_isuid) = host.s_isuid
  let is_sgid   ~host code = (code land host.s_isgid) = host.s_isgid
  let is_sticky ~host code = (code land host.s_isvtx) = host.s_isvtx

  let string_of_rwx xc rwx =
    let s = String.create 3 in
    String.blit
      (match rwx lsr 1 with 3 -> "rw" | 2 -> "r-" | 1 -> "-w" | _ -> "--")
      0 s 0 2;
    s.[2] <- begin
      if rwx land 1 = 1 then xc
      else if xc='x' then '-'
      else Char.uppercase xc
    end; s
  let to_string ~host code =
    let u = mask_and_rshift host.access_mask host.s_irwxu in
    let g = mask_and_rshift host.access_mask host.s_irwxg in
    let o = mask_and_rshift host.access_mask host.s_irwxo in
    let rwx xc mask = string_of_rwx xc (mask_and_rshift mask code) in
    let s = String.create 9 in
    String.blit
      (rwx (if is_suid ~host code   then 's' else 'x') u)
      0 s 0 3;
    String.blit
      (rwx (if is_sgid ~host code   then 's' else 'x') g)
      0 s 3 3;
    String.blit
      (rwx (if is_sticky ~host code then 't' else 'x') o)
      0 s 6 3;
    s
end

module Mode = struct
  type t = File_kind.t * File_perm.t

  type host = {
    file_kind : File_kind.host;
    file_perm : File_perm.host;
  }
  let host = {
    file_kind = File_kind.host;
    file_perm = File_perm.host;
  }

  let to_string ~host (k,p) =
    let ps = File_perm.to_string ~host:host.file_perm p in
    let lps = String.length ps in
    let s = String.(create (lps + 1)) in
    s.[0] <- File_kind.to_char k;
    String.blit ps 0 s 1 lps;
    s

  let to_code ~host (kind, perm) =
    (File_kind.to_code ~host:host.file_kind kind) lor perm

  let of_code_exn ~host code =
    let (k_defns,_) = host.file_kind in
    let unknown = lnot (k_defns.File_kind.s_ifmt
                        lor host.file_perm.File_perm.full_mask) in
    let remaining = code land unknown in
    if remaining <> 0
    then raise (Failure (Printf.sprintf "unknown mode bits 0x%X" remaining))
    else
      File_kind.of_code_exn ~host:host.file_kind code,
      File_perm.full_of_code ~host:host.file_perm code
end

type host = {
  file_kind : File_kind.host;
  file_perm : File_perm.host;
  mode      : Mode.host;
}
let host = {
  file_kind = File_kind.host;
  file_perm = File_perm.host;
  mode      = Mode.host;
}
