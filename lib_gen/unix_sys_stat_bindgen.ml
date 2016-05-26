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

module Prefixed_bindings(F: Cstubs.FOREIGN) =
struct
  include Unix_sys_stat_bindings.C(
    struct
      include F
      let foreign f = F.foreign ("unix_sys_stat_"^ f)
    end
    )
end

type configuration = {
  errno: Cstubs.errno_policy;
  concurrency: Cstubs.concurrency_policy;
  headers: string;
  bindings: (module Cstubs.BINDINGS)
}

let standard_configuration = {
  errno = Cstubs.ignore_errno;
  concurrency = Cstubs.sequential;
  headers = "#include \"unix_sys_stat_util.h\"";
  bindings = (module Prefixed_bindings)
}

let lwt_configuration = {
  errno = Cstubs.return_errno;
  concurrency = Cstubs.lwt_jobs;
  headers = "#include <sys/stat.h>";
  bindings = (module Unix_sys_stat_bindings.C)
}

let configuration = ref standard_configuration
let ml_file = ref ""
let c_file = ref ""

let argspec : (Arg.key * Arg.spec * Arg.doc) list = [
  "--ml-file", Arg.Set_string ml_file, "set the ML output file";
  "--c-file", Arg.Set_string c_file, "set the C output file";
  "--lwt-bindings", Arg.Unit (fun () -> configuration := lwt_configuration),
  "generate Lwt jobs bindings";
]

let () =
  let () = Arg.parse argspec failwith "" in
  if !ml_file = "" || !c_file = "" then
    failwith "Both --ml-file and --c-file arguments must be supplied";
  let {errno; concurrency; headers; bindings} = !configuration in
  let prefix = "caml_" in
  let stubs_oc = open_out !c_file in
  let fmt = Format.formatter_of_out_channel stubs_oc in
  Format.fprintf fmt "%s@." headers;
  Cstubs.write_c ~errno ~concurrency fmt ~prefix bindings;
  close_out stubs_oc;

  let generated_oc = open_out !ml_file in
  let fmt = Format.formatter_of_out_channel generated_oc in
  Cstubs.write_ml ~errno ~concurrency fmt ~prefix bindings;
  close_out generated_oc
