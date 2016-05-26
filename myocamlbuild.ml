open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

let ctypes_libdir = Sys.getenv "CTYPES_LIB_DIR" in
let lwt_libdir = Sys.getenv "LWT_LIB_DIR" in
let ocaml_libdir = Sys.getenv "OCAML_LIB_DIR" in

dispatch begin
  function
  | After_rules ->

    rule "cstubs: lib_gen/x_types_detect.c -> x_types_detect"
      ~prods:["lib_gen/%_types_detect"]
      ~deps:["lib_gen/%_types_detect.c"]
      (fun env build ->
         Cmd (S[A"cc";
                A("-I"); A ctypes_libdir;
                A("-I"); A ocaml_libdir;
                A"-o";
                A(env "lib_gen/%_types_detect");
                A(env "lib_gen/%_types_detect.c");
               ]));

    rule "cstubs: lib_gen/x_types_detect -> unix/x_types_detected.ml"
      ~prods:["unix/%_types_detected.ml"]
      ~deps:["lib_gen/%_types_detect"]
      (fun env build ->
         Cmd (S[A(env "lib_gen/%_types_detect");
                Sh">";
                A(env "unix/%_types_detected.ml");
               ]));

    rule "cstubs: lib_gen/x_types.ml -> x_types_detect.c"
      ~prods:["lib_gen/%_types_detect.c"]
      ~deps: ["lib_gen/%_typegen.byte"]
      (fun env build ->
         Cmd (A(env "lib_gen/%_typegen.byte")));

    copy_rule "cstubs: lib_gen/x_types.ml -> unix/x_types.ml"
      "lib_gen/%_types.ml" "unix/%_types.ml";

    rule "cstubs: unix/x_bindings.ml -> x_stubs.c, x_generated.ml"
      ~prods:["unix/%_stubs.c"; "unix/%_generated.ml"]
      ~deps: ["lib_gen/%_bindgen.byte"]
      (fun env build ->
        Cmd (S[A(env "lib_gen/%_bindgen.byte");
               A"--c-file";
               A(env "unix/%_stubs.c");
                 A"--ml-file";
               A(env "unix/%_generated.ml")]));

    rule "sys_stat_maps: maps/x -> lib/sys_stat_map_x.ml"
      ~prods:["lib/sys_stat_map_%.ml"]
      ~deps: ["src/sys_stat_srcgen.byte"; "maps/%"]
      (fun env build ->
         Cmd (S[A"src/sys_stat_srcgen.byte";
                Sh"<";
                A(env "maps/%");
                Sh">";
                A(env "lib/sys_stat_map_%.ml");
               ]));

    copy_rule "cstubs: lib_gen/x_bindings.ml -> unix/x_bindings.ml"
      "lib_gen/%_bindings.ml" "unix/%_bindings.ml";

    flag ["c"; "compile"] & S[A"-ccopt"; A"-I/usr/local/include"];
    flag ["c"; "ocamlmklib"] & A"-L/usr/local/lib";
    flag ["ocaml"; "link"; "native"; "program"] &
      S[A"-cclib"; A"-L/usr/local/lib"];

    (* Linking cstubs *)
    dep ["c"; "compile"; "use_sys_stat_util"]
      ["unix/unix_sys_stat_util.o"; "unix/unix_sys_stat_util.h"];
    flag ["c"; "compile"; "use_ctypes"] & S[A"-I"; A ctypes_libdir];
    flag ["c"; "compile"; "use_lwt"] & S[A"-I"; A lwt_libdir];
    flag ["c"; "compile"; "debug"] & A"-g";

    (* Linking generated stubs *)
    dep ["ocaml"; "link"; "byte"; "library"; "use_sys_stat_stubs"]
      ["unix/dllunix_sys_stat_stubs"-.-(!Options.ext_dll)];
    flag ["ocaml"; "link"; "byte"; "library"; "use_sys_stat_stubs"] &
      S[A"-dllib"; A"-lunix_sys_stat_stubs"];

    dep ["ocaml"; "link"; "native"; "library"; "use_sys_stat_stubs"]
      ["unix/libunix_sys_stat_stubs"-.-(!Options.ext_lib)];
    flag ["ocaml"; "link"; "native"; "library"; "use_sys_stat_stubs"] &
      S[A"-cclib"; A"-lunix_sys_stat_stubs"];

    flag ["ocaml"; "link"; "byte"; "library"; "use_sys_stat_lwt_stubs"] &
      S[A"-dllib"; A"-lunix_sys_stat_lwt_stubs"];
    flag ["ocaml"; "link"; "native"; "library"; "use_sys_stat_lwt_stubs"] &
      S[A"-cclib"; A"-lunix_sys_stat_lwt_stubs"];

    (* Linking tests *)
    flag ["ocaml"; "link"; "byte"; "program"; "use_sys_stat_stubs"] &
      S[A"-dllib"; A"-lunix_sys_stat_stubs"; A"-I"; A"unix/"];
    dep ["ocaml"; "link"; "native"; "program"; "use_sys_stat_stubs"]
      ["unix/libunix_sys_stat_stubs"-.-(!Options.ext_lib)];

    flag ["ocaml"; "link"; "byte"; "program"; "use_sys_stat_lwt_stubs"] &
      S[A"-dllib"; A"-lunix_lwt_sys_stat_stubs"];
  | _ -> ()
end;;
