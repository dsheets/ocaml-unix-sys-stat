(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

open Ctypes

module C(F: Cstubs.Types.TYPE) = struct

  module File_kind = struct
    let s_ifmt   = F.(constant "S_IFMT" int)
    let s_ifdir  = F.(constant "S_IFDIR" int)
    let s_ifchr  = F.(constant "S_IFCHR" int)
    let s_ifblk  = F.(constant "S_IFBLK" int)
    let s_ifreg  = F.(constant "S_IFREG" int)
    let s_ififo  = F.(constant "S_IFIFO" int)
    let s_iflnk  = F.(constant "S_IFLNK" int)
    let s_ifsock = F.(constant "S_IFSOCK" int)
  end

  module File_perm = struct
    let s_irwxu = F.(constant "S_IRWXU" int)
    let s_irwxg = F.(constant "S_IRWXG" int)
    let s_irwxo = F.(constant "S_IRWXO" int)
    let s_isuid = F.(constant "S_ISUID" int)
    let s_isgid = F.(constant "S_ISGID" int)
    let s_isvtx = F.(constant "S_ISVTX" int)
  end

  module At = struct
    let at_symlink_nofollow = F.(constant "AT_SYMLINK_NOFOLLOW" int)
  end

  module Stat = struct
    open Posix_types

    type t

    let t : t structure F.typ = F.structure "stat"
    let ( -:* ) s x = F.field t s (F.lift_typ x)
    let st_dev       = "st_dev"       -:* dev_t
    let st_ino       = "st_ino"       -:* ino_t
    let st_nlink     = "st_nlink"     -:* nlink_t
    let st_mode      = "st_mode"      -:* mode_t
    let st_uid       = "st_uid"       -:* uid_t
    let st_gid       = "st_gid"       -:* gid_t
    let st_rdev      = "st_rdev"      -:* dev_t
    let st_size      = "st_size"      -:* off_t
    let st_blksize   = "st_blksize"   -:* blksize_t
    let st_blocks    = "st_blocks"    -:* blkcnt_t
    let st_atime     = "st_atime"     -:* time_t
    (*let atimensec = "atimensec" -:* uint32_t (* Linux only? *)*)
    let st_mtime     = "st_mtime"     -:* time_t
    (*let mtimensec = "mtimensec" -:* uint32_t (* Linux only? *)*)
    let st_ctime     = "st_ctime"     -:* time_t
    (*let ctimensec = "ctimensec" -:* uint32_t (* Linux only? *)*)
    let () = F.seal t
  end
end
