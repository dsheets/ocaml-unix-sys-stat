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

let split_length_prefixed s =
  Scanf.sscanf s ":%d\n%n"
    (fun len rest ->
       (String.sub s 0 (len+rest),
        String.sub s (rest + len) (String.length s - len - rest)))

let write_length_prefixed s =
  Printf.sprintf ":%d\n%s" (String.length s) s

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

  let of_char = function
    | '-' -> Some REG
    | 'd' -> Some DIR
    | 'c' -> Some CHR
    | 'b' -> Some BLK
    | 'l' -> Some LNK
    | 'p' -> Some FIFO
    | 's' -> Some SOCK
    | _   -> None

  let of_char_exn = function
    | '-' -> REG
    | 'd' -> DIR
    | 'c' -> CHR
    | 'b' -> BLK
    | 'l' -> LNK
    | 'p' -> FIFO
    | 's' -> SOCK
    | _   -> failwith "Sys_Stat.File_kind.of_char_exn"

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

    let to_string (defns, _) =
      let buf = Buffer.create 128 in
      Buffer.add_string buf (Printf.sprintf "S_IFMT\t%x\n"   defns.mask);
      Buffer.add_string buf (Printf.sprintf "S_IFDIR\t%x\n"  defns.dir);
      Buffer.add_string buf (Printf.sprintf "S_IFCHR\t%x\n"  defns.chr);
      Buffer.add_string buf (Printf.sprintf "S_IFBLK\t%x\n"  defns.blk);
      Buffer.add_string buf (Printf.sprintf "S_IFREG\t%x\n"  defns.reg);
      Buffer.add_string buf (Printf.sprintf "S_IFIFO\t%x\n"  defns.fifo);
      Buffer.add_string buf (Printf.sprintf "S_IFLNK\t%x\n"  defns.lnk);
      Buffer.add_string buf (Printf.sprintf "S_IFSOCK\t%x\n" defns.sock);
      Buffer.contents buf

    let of_string s =
      Scanf.sscanf s
        "S_IFMT\t%x\n\
         S_IFDIR\t%x\n\
         S_IFCHR\t%x\n\
         S_IFBLK\t%x\n\
         S_IFREG\t%x\n\
         S_IFIFO\t%x\n\
         S_IFLNK\t%x\n\
         S_IFSOCK\t%x\n"
        (fun mask  dir  chr  blk  reg  fifo  lnk  sock -> of_defns {
           mask; dir; chr; blk; reg; fifo; lnk; sock;
         })
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

    let to_string { rwxu; rwxg; rwxo; suid; sgid; svtx; } =
      let buf = Buffer.create 128 in
      Buffer.add_string buf (Printf.sprintf "S_IRWXU\t%x\n" rwxu);
      Buffer.add_string buf (Printf.sprintf "S_IRWXG\t%x\n" rwxg);
      Buffer.add_string buf (Printf.sprintf "S_IRWXO\t%x\n" rwxo);
      Buffer.add_string buf (Printf.sprintf "S_ISUID\t%x\n" suid);
      Buffer.add_string buf (Printf.sprintf "S_ISGID\t%x\n" sgid);
      Buffer.add_string buf (Printf.sprintf "S_ISVTX\t%x\n" svtx);
      Buffer.contents buf

    let of_string s =
      Scanf.sscanf s
        "S_IRWXU\t%x\n\
         S_IRWXG\t%x\n\
         S_IRWXO\t%x\n\
         S_ISUID\t%x\n\
         S_ISGID\t%x\n\
         S_ISVTX\t%x\n"
        (fun rwxu rwxg rwxo suid sgid svtx ->
           let access_mask = rwxu lor rwxg lor rwxo in
           let full_mask = access_mask lor suid lor sgid lor svtx in
           {
             access_mask; full_mask;
             rwxu; rwxg; rwxo; suid; sgid; svtx
           }
        )
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

  let string_of_rwx xc rwx = (* xc is the value of the x position where the executable bit is set *)
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

  let of_string_failure () =
    invalid_arg "Sys_Stat.File_perm.of_string"

  type special = Nothing | Setuid | Sticky

  let rwx_of_string s i =
    let n = match s.[i+0] with
      | 'r' -> 4
      | '-' -> 0
      | _ -> of_string_failure ()
    in
    let n = match s.[i+1] with
      | 'w' -> 2 lor n
      | '-' -> n
      | _ -> of_string_failure () in
    let n = match s.[i+2] with
      | 'x'|'s'|'t' -> 1 lor n
      | '-'|'S'|'T' -> n
      | _ -> of_string_failure ()
    in
    let special = match s.[i+2] with
      | 'x'|'-' -> Nothing
      | 's'|'S' -> Setuid
      | 't'|'T' -> Sticky
      | _ -> of_string_failure ()
    in
    (n, special)

  let of_string ~host str =
    let umask = mask_and_rshift host.access_mask host.rwxu
    and gmask = mask_and_rshift host.access_mask host.rwxg
    and omask = mask_and_rshift host.access_mask host.rwxo
    and suidmask = mask_and_rshift host.full_mask host.suid
    and sgidmask = mask_and_rshift host.full_mask host.sgid
    and stickymask = mask_and_rshift host.full_mask host.svtx
    and u, extrau = rwx_of_string str 0
    and g, extrag = rwx_of_string str 3
    and o, extrao = rwx_of_string str 6 in
    let suid = match extrau with
      | Setuid -> suidmask
      | Nothing -> 0o0
      | Sticky -> of_string_failure () in
    let sgid = match extrag with
      | Setuid -> sgidmask
      | Nothing -> 0o0
      | Sticky -> of_string_failure () in
    let sticky = match extrao with
      | Setuid -> of_string_failure ()
      | Nothing -> 0o0
      | Sticky -> stickymask in
      0o0
      lor (inject_and_lshift umask u)
      lor (inject_and_lshift gmask g)
      lor (inject_and_lshift omask o)
      lor suid
      lor sgid
      lor sticky
end

module Mode = struct
  type t = File_kind.t * File_perm.t

  module Host = struct
    type t = {
      file_kind : File_kind.Host.t;
      file_perm : File_perm.Host.t;
    }

    let to_string { file_kind; file_perm } =
      let kind_s = File_kind.Host.to_string file_kind in
      let kind_l = String.length kind_s in
      let perm_s = File_perm.Host.to_string file_perm in
      let perm_l = String.length perm_s in
      let buf = Buffer.create (kind_l + perm_l + 12) in
      Buffer.add_char buf ':';
      Buffer.add_string buf (string_of_int kind_l);
      Buffer.add_char buf '\n';
      Buffer.add_string buf kind_s;
      Buffer.add_char buf ':';
      Buffer.add_string buf (string_of_int perm_l);
      Buffer.add_char buf '\n';
      Buffer.add_string buf perm_s;
      Buffer.contents buf

    let of_string s =
      let kind_ls = Scanf.sscanf s ":%s\n" (fun x -> x) in
      let kind_l = int_of_string kind_ls in
      let kind_lsl = 2 + String.length kind_ls in
      let kind_s = String.sub s kind_lsl kind_l in
      let file_kind = File_kind.Host.of_string kind_s in
      let rest_o = kind_lsl + kind_l in
      let rest = String.sub s rest_o (String.length s - rest_o) in
      let perm_ls = Scanf.sscanf rest ":%s\n" (fun x -> x) in
      let perm_l = int_of_string perm_ls in
      let perm_lsl = 2 + String.length perm_ls in
      let perm_s = String.sub rest perm_lsl perm_l in
      let file_perm = File_perm.Host.of_string perm_s in
      let rest_o = perm_lsl + perm_l in
      assert (rest_o = String.length rest);
      { file_kind; file_perm }

  end

  let to_string ~host (k,p) =
    let ps = File_perm.to_string ~host:host.Host.file_perm p in
    let lps = String.length ps in
    let s = Bytes.create (lps + 1) in
    Bytes.set s 0 (File_kind.to_char k);
    String.blit ps 0 s 1 lps;
    Bytes.to_string s
      
  let of_string_failure () =
    invalid_arg "Sys_Stat.Mode.of_string"

  let of_string ~host s =
    let h, t = s.[0], String.sub s 1 (pred (String.length s)) in
    let p = File_perm.of_string ~host:host.Host.file_perm t in
    match File_kind.of_char h with
    | None -> of_string_failure ()
    | Some k -> (k, p)

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

  let of_code ~host code =
    try Some (of_code_exn ~host code) with Not_found -> None
end

module At = struct

  type t = Symlink_nofollow

  type defns = {
    symlink_nofollow: int
  }

  module Host = struct
    type t = defns
    let of_defns d = d
    let to_defns d = d

    let to_string { symlink_nofollow } =
      write_length_prefixed
        (Printf.sprintf "AT_SYMLINK_NOFOLLOW\t%x\n" symlink_nofollow)

    let of_string s =
      let map, _ = split_length_prefixed s in
      Scanf.sscanf map ":%d\nAT_SYMLINK_NOFOLLOW\t%x\n"
        (fun _ symlink_nofollow -> { symlink_nofollow })
  end

  let to_string = function
    | Symlink_nofollow -> "Symlink_nofollow"

  let of_string = function
    | "Symlink_nofollow" -> Symlink_nofollow
    | _                  -> invalid_arg "Sys_stat.At.of_string"

  let to_code ~host:{symlink_nofollow} = function
    Symlink_nofollow -> symlink_nofollow

  let of_code_exn ~host:{symlink_nofollow} code =
    if code = symlink_nofollow then Symlink_nofollow
    else raise Not_found

  let of_code ~host code =
    try Some (of_code_exn ~host code) with Not_found -> None
end

module Host = struct
  type t = {
    file_kind : File_kind.Host.t;
    file_perm : File_perm.Host.t;
    mode      : Mode.Host.t;
    at        : At.Host.t;
  }

  let file_kind { file_kind } = file_kind
  let file_perm { file_perm } = file_perm
  let mode { mode } = mode

  let to_string { mode; at } =
    At.Host.to_string at ^ Mode.Host.to_string mode

  let of_string s =
    let atbit, rest = split_length_prefixed s in
    let mode = Mode.Host.of_string rest in
    let at   = At.Host.of_string atbit in
    {
      file_kind = mode.Mode.Host.file_kind;
      file_perm = mode.Mode.Host.file_perm;
      mode;
      at;
    }
end
