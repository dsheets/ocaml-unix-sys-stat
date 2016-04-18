(*
 * Copyright (c) 2016 Jeremy Yallop <yallop@docker.com>
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

module File_perm =
struct
  let string_test_cases = Sys_stat.File_kind.([
      0o001, "--------x";
      0o010, "-----x---";
      0o100, "--x------";
      0o002, "-------w-";
      0o020, "----w----";
      0o200, "-w-------";
      0o004, "------r--";
      0o040, "---r-----";
      0o400, "r--------";
    ])

  let test_of_string_linux () =
    let host = Sys_stat_host.Linux.V4_1_12.Musl.v1_1_12 in
    let of_string = Sys_stat.File_perm.of_string ~host:host.Sys_stat.Host.file_perm in
    ListLabels.iter string_test_cases
      ~f:(fun (expected, string) ->
          Printf.ksprintf Alcotest.(check int)
            "%s parses as Oo%o" string expected
            expected
            (of_string string))

  let test_to_string_linux () =
    let host = Sys_stat_host.Linux.V4_1_12.Musl.v1_1_12 in
    let to_string = Sys_stat.File_perm.to_string ~host:host.Sys_stat.Host.file_perm in
    ListLabels.iter string_test_cases
      ~f:(fun (perm, expected) ->
          Printf.ksprintf Alcotest.(check string)
            "Oo%o prints as %s" perm expected
            expected
            (to_string perm))

  let test_roundtrip_linux () =
    let host = Sys_stat_host.Linux.V4_1_12.Musl.v1_1_12 in
    ArrayLabels.iter Permission_strings.permission_strings
      ~f:(fun s ->
          Printf.ksprintf Alcotest.(check string) 
            "to_string (of_string %s) == %s" s s
            s
            (Sys_stat.File_perm.to_string ~host:host.Sys_stat.Host.file_perm
               (Sys_stat.File_perm.of_string ~host:host.Sys_stat.Host.file_perm s)))

  let tests = [
    "of_string", `Quick, test_of_string_linux;
    "to_string", `Quick, test_to_string_linux;
    "roundtrip", `Quick, test_roundtrip_linux;
  ]
end


module Mode =
struct
  let test_to_string_linux () =
    let host = Sys_stat_host.Linux.V4_1_12.Musl.v1_1_12 in
    let to_string = Sys_stat.Mode.to_string ~host:host.Sys_stat.Host.mode in
    let tests = Sys_stat.File_kind.([
      DIR , 0o001, "d--------x";
      CHR , 0o010, "c-----x---";
      BLK , 0o100, "b--x------";
      REG , 0o002, "--------w-";
      FIFO, 0o020, "p----w----";
      LNK , 0o200, "l-w-------";
      SOCK, 0o004, "s------r--";
      SOCK, 0o040, "s---r-----";
      SOCK, 0o400, "sr--------";
    ]) in
    ListLabels.iter tests
      ~f:(fun (kind, perm, expected) ->
          Printf.ksprintf Alcotest.(check string)
            "(%s, Oo%o) prints as %s"
            (Sys_stat.File_kind.to_string kind) perm expected
            expected
            (to_string (kind, perm)))
      

  let tests = [
    "to_string", `Quick, test_to_string_linux;
  ]
end


let tests = [
  "file_perm", File_perm.tests;
  "mode", Mode.tests;
]


let () = Alcotest.run "Sys_stat" tests
