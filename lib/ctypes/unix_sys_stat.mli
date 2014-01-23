include module type of Unix_sys_stat_common

open PosixTypes

(** Can raise Unix.Unix_error *)
val mknod : string -> mode_t -> dev_t -> unit
