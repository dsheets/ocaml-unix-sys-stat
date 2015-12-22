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
  type t =
    | DIR
    | CHR
    | BLK
    | REG
    | FIFO
    | LNK
    | SOCK

  type defns = {
    mask : int;
    dir  : int;
    chr  : int;
    blk  : int;
    reg  : int;
    fifo : int;
    lnk  : int;
    sock : int;
  }

  type index = (int, t) Hashtbl.t

  let to_char = function
    | REG   -> '-'
    | DIR   -> 'd'
    | CHR   -> 'c'
    | BLK   -> 'b'
    | LNK   -> 'l'
    | FIFO  -> 'p'
    | SOCK  -> 's'

  let to_string = function
    | REG   -> "REG"
    | DIR   -> "DIR"
    | CHR   -> "CHR"
    | BLK   -> "BLK"
    | LNK   -> "LNK"
    | FIFO  -> "FIFO"
    | SOCK  -> "SOCK"

  let to_code ~host = let (defns,_) = host in function
    | REG   -> defns.reg
    | DIR   -> defns.dir
    | CHR   -> defns.chr
    | BLK   -> defns.blk
    | LNK   -> defns.lnk
    | FIFO  -> defns.fifo
    | SOCK  -> defns.sock

  module Host = struct
    type t = defns * index

    let index_of_defns defns =
      let open Hashtbl in
      let h = create 10 in
      replace h defns.reg  REG;
      replace h defns.dir  DIR;
      replace h defns.chr  CHR;
      replace h defns.blk  BLK;
      replace h defns.lnk  LNK;
      replace h defns.fifo FIFO;
      replace h defns.sock SOCK;
      h

    let of_defns defns = (defns, index_of_defns defns)

    let to_defns (defns, _) = defns
  end

  let of_code_exn ~host code =
    let (defns,index) = host in
    Hashtbl.find index (code land defns.mask)

  let of_code ~host code =
    try Some (of_code_exn ~host code) with Not_found -> None
end

module File_perm = struct
  type t = int

  type defns = {
    access_mask : int;
    full_mask   : int;

    rwxu     : int;
    rwxg     : int;
    rwxo     : int;

    suid     : int;
    sgid     : int;
    svtx     : int;
  }

  module Host = struct
    type t = defns

    let of_defns defns = defns
    let to_defns defns = defns
  end

  let mask_and_rshift mask code =
    (code land mask) lsr (find_least_one (Int32.of_int mask))

  let inject_and_lshift mask perm =
    perm lsl (find_least_one (Int32.of_int mask))

  (* TODO: these should standardize the bits *)
  let access_of_code ~host code = mask_and_rshift host.access_mask code
  let full_of_code   ~host code = mask_and_rshift host.full_mask   code
  let to_code        ~host perm = inject_and_lshift host.full_mask perm

  let is_suid   ~host code = (code land host.suid) = host.suid
  let is_sgid   ~host code = (code land host.sgid) = host.sgid
  let is_sticky ~host code = (code land host.svtx) = host.svtx

  let string_of_rwx xc rwx =
    let s = Bytes.create 3 in
    String.blit
      (match rwx lsr 1 with 3 -> "rw" | 2 -> "r-" | 1 -> "-w" | _ -> "--")
      0 s 0 2;
    Bytes.set s 2 begin
      if rwx land 1 = 1 then xc
      else if xc='x' then '-'
      else Char.uppercase xc
    end;
    s

  let to_string ~host code =
    let u = mask_and_rshift host.access_mask host.rwxu in
    let g = mask_and_rshift host.access_mask host.rwxg in
    let o = mask_and_rshift host.access_mask host.rwxo in
    let rwx xc mask = string_of_rwx xc (mask_and_rshift mask code) in
    let s = Bytes.create 9 in
    Bytes.blit
      (rwx (if is_suid ~host code   then 's' else 'x') u)
      0 s 0 3;
    Bytes.blit
      (rwx (if is_sgid ~host code   then 's' else 'x') g)
      0 s 3 3;
    Bytes.blit
      (rwx (if is_sticky ~host code then 't' else 'x') o)
      0 s 6 3;
    Bytes.to_string s
end

module Mode = struct
  type t = File_kind.t * File_perm.t

  module Host = struct
    type t = {
      file_kind : File_kind.Host.t;
      file_perm : File_perm.Host.t;
    }
  end

  let to_string ~host (k,p) =
    let ps = File_perm.to_string ~host:host.Host.file_perm p in
    let lps = String.length ps in
    let s = Bytes.create (lps + 1) in
    Bytes.set s 0 (File_kind.to_char k);
    String.blit ps 0 s 1 lps;
    Bytes.to_string s

  let to_code ~host (kind, perm) =
    (File_kind.to_code ~host:host.Host.file_kind kind) lor perm

  let of_code_exn ~host code =
    let (k_defns,_) = host.Host.file_kind in
    let unknown = lnot (k_defns.File_kind.mask
                        lor host.Host.file_perm.File_perm.full_mask) in
    let remaining = code land unknown in
    if remaining <> 0
    then raise (Failure (Printf.sprintf "unknown mode bits 0x%X" remaining))
    else
      File_kind.of_code_exn ~host:host.Host.file_kind code,
      File_perm.full_of_code ~host:host.Host.file_perm code
end

module Host = struct
  type t = {
    file_kind : File_kind.Host.t;
    file_perm : File_perm.Host.t;
    mode      : Mode.Host.t;
  }

  let file_kind { file_kind } = file_kind
  let file_perm { file_perm } = file_perm
  let mode { mode } = mode
end
