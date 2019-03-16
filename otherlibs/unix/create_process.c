// Contributed by James Woodyatt <jhw@conjury.org>
// March 2019

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/debugger.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include <caml/fail.h>

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>
#include <signal.h>
#include <errno.h>

CAMLprim value unix_execvp(value path, value args);
CAMLprim value unix_execvpe(value path, value args, value envs);

static int prelim(value fin, value fout, value ferr)
{
  int ret;
  int fdv[3] = { Int_val(fin), Int_val(fout), Int_val(ferr) };

  for (int i = 0; i < 3; ++i) {
    int *pfd = &fdv[i];
    while (*pfd < 3) {
      ret = dup(*pfd);
      if (ret < 0) return -1;
      *pfd = ret;
    }
  }

  for (int i = 0; i < 3; ++i) {
    const int fd = fdv[i];

    if (fd == i) {
      ret = fcntl(fd, F_GETFD, 0);
      if (ret == -1) return -1;
      ret = fcntl(fd, F_SETFD, ret & ~FD_CLOEXEC);
      if (ret == -1) return -1;
    }
    else {
      ret = dup2(fd, i);
      if (ret < 0) return -1;
    }
  }

  for (int i = 0; i < 3; ++i)
    close(fdv[i]);

  sigset_t ss;
  sigemptyset(&ss);
  ret = sigprocmask(SIG_SETMASK, &ss, NULL);
  if (ret != 0) return -1;

  return 0;
}

CAMLprim value unix_fork_and_execvp(value path, value args, value fin,
  value fout, value ferr)
{
  char_os * wpath;
  char_os ** argv;
  int ret;

  caml_unix_check_path(path, "execvp");
  argv = cstringvect(args, "execvp");
  wpath = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();

  ret = fork();
  if (ret == 0) {
    ret = prelim(fin, fout, ferr);
    if (ret == 0)
      ret = execvp_os(wpath, EXECV_CAST argv);

    _exit(128);
  }

  caml_leave_blocking_section();
  caml_stat_free(wpath);
  cstringvect_free(argv);

  if (ret == -1) uerror("fork", Nothing);

  if (caml_debugger_in_use)
    if ((caml_debugger_fork_mode && ret == 0) ||
        (!caml_debugger_fork_mode && ret != 0))
      caml_debugger_cleanup_fork();

  return Val_int(ret);
}

#ifdef HAS_EXECVPE

CAMLprim value unix_fork_and_execvpe(value path, value args, value envs,
  value fin, value fout, value ferr)
{
  char_os * wpath;
  char_os ** argv;
  char_os ** envp;
  int ret;

  caml_unix_check_path(path, "execvpe");
  argv = cstringvect(args, "execvpe");
  envp = cstringvect(envs, "execvpe");
  wpath = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();

  ret = fork();
  if (ret == 0) {
    ret = prelim(fin, fout, ferr);
    if (ret == 0)
      execvpe_os(wpath, EXECV_CAST argv, EXECV_CAST envp);
    _exit(128);
  }

  caml_leave_blocking_section();
  caml_stat_free(wpath);
  cstringvect_free(envp);
  cstringvect_free(argv);

  if (ret == -1) uerror("fork", Nothing);

  if (caml_debugger_in_use)
    if ((caml_debugger_fork_mode && ret == 0) ||
        (!caml_debugger_fork_mode && ret != 0))
      caml_debugger_cleanup_fork();

  return Val_int(ret);
}

#else

CAMLprim value unix_fork_and_execvpe(value path, value args, value envs,
  value fin, value fout, value ferr)
{
  unix_error(ENOSYS, "execvpe", path);
  return Val_unit;
}

#endif

CAMLprim value unix_fork_and_execvpe_bytecode(value * argv, int argn)
{
  return unix_fork_and_execvpe(argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5]);
}

CAMLprim value unix_fork_and_execve(value path, value args,
  value envs, value fin, value fout, value ferr)
{
  char_os * wpath;
  char_os ** argv;
  char_os ** envp;
  int ret;

  caml_unix_check_path(path, "execve");
  argv = cstringvect(args, "execve");
  envp = cstringvect(envs, "execve");
  wpath = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();

  ret = fork();
  if (ret == 0) {
    ret = prelim(fin, fout, ferr);
    if (ret == 0)
      execve_os(wpath, EXECV_CAST argv, EXECV_CAST envp);
    _exit(128);
  }

  caml_leave_blocking_section();
  caml_stat_free(wpath);
  cstringvect_free(envp);
  cstringvect_free(argv);

  if (ret == -1) uerror("fork", Nothing);

  if (caml_debugger_in_use)
    if ((caml_debugger_fork_mode && ret == 0) ||
        (!caml_debugger_fork_mode && ret != 0))
      caml_debugger_cleanup_fork();

  return Val_int(ret);
}

CAMLprim value unix_fork_and_execve_bytecode(value * argv, int argn)
{
  return unix_fork_and_execve(argv[0], argv[1], argv[2], argv[3],
    argv[4], argv[5]);
}
