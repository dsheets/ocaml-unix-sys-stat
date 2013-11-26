type t = Unix.stats

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

  let of_code ~host code =
    let (_,index) = host in
    try Some (Hashtbl.find index code)
    with Not_found -> None
end
