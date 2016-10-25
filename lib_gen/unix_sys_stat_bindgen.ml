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

let () = Ctypes_stub_generator.main
    [ { Ctypes_stub_generator.name = "unix";
        errno = Cstubs.ignore_errno;
        concurrency = Cstubs.sequential;
        headers = "#include \"unix_sys_stat_util.h\"";
        bindings = (module Unix_sys_stat_bindings.Prefixed_C) };
      
      { Ctypes_stub_generator.name = "lwt";
        errno = Cstubs.return_errno;
        concurrency = Cstubs.lwt_jobs;
        headers = "#include <sys/stat.h>";
        bindings = (module Unix_sys_stat_bindings.C) } ]
