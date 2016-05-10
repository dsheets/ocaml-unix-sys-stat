/*
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
 */

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "caml/memory.h"
#include "caml/alloc.h"

#include "ctypes_raw_pointer.h"

#include "lwt_unix.h"

struct job_lstat {
  struct lwt_unix_job job;
  struct stat *stat;
  value stat_value;
  char *path;
  int error_code;
  int successful;
};

static void worker_lstat(struct job_lstat *job)
{
  job->successful = lstat(job->path, job->stat) != -1;
  job->error_code = errno; 
}

static value result_lstat(struct job_lstat *job)
{
  CAMLparam0 ();

  CAMLlocal2 (stat_value, result);
  stat_value = job->stat_value;
  caml_remove_generational_global_root(&job->stat_value);
  free(job->path);

  if (job->successful) {
    /*  Stat_info of Sys_stat_unix.Stat.t */
    result = caml_alloc(1, 0);
    Store_field(result, 0, stat_value);
  }
  else {
    /* Stat_error of int */
    result = caml_alloc(1, 1);
    Store_field(result, 0, Val_int(job->error_code));
  }
  lwt_unix_free_job(&job->job);
  CAMLreturn (result);
}


CAMLprim
value unix_sys_stat_lwt_lstat_job(value path, value stat, value pointer)
{
  CAMLparam2 (path, stat);

  LWT_UNIX_INIT_JOB(job, lstat, 0);

  job->stat = (void *)Nativeint_val(pointer);
  job->stat_value = stat;
  caml_register_generational_global_root(&job->stat_value);
  job->path = strdup(String_val(path));
  job->error_code = 0;
  job->successful = 1;

  CAMLreturn(lwt_unix_alloc_job(&(job->job)));
}
