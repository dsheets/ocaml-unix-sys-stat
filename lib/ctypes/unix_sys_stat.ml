include Unix_sys_stat_common

open Ctypes
open Foreign
open PosixTypes

external unix_sys_stat_mknod_ptr : unit -> int64 = "unix_sys_stat_mknod_ptr"

let mknod =
  let c = coerce
    (ptr void)
    (funptr ~check_errno:true (string @-> mode_t @-> dev_t @-> returning int))
    (ptr_of_raw_address (unix_sys_stat_mknod_ptr ()))
  in
  fun pathname mode dev -> ignore (c pathname mode dev)
