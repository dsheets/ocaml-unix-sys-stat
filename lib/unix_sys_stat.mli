type t = Unix.stats

module File_kind : sig
  type t = Unix.file_kind

  type host

  val host : host

  val to_code : host:host -> t -> int option
  val of_code : host:host -> int -> t option

  val to_string : t -> string
end

