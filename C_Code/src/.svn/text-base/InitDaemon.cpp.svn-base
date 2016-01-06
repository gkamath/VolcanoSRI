#include <unistd.h> 
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>

#define NULL_DESC "/dev/null"
#define RUN_DIR "./"

/**
 * initialize the program as a linux daemon
 */
int init_daemon(void)
{
  pid_t pid;
  int i;
  int fd;
  
  fd = open(NULL_DESC, O_RDWR);
  if(fd < 0)
    return -1;
  
  dup2(fd, 0);
  dup2(fd, 1);
  dup2(fd, 2);
 
  /// parent exits , child continues
  if((pid = fork()) < 0)
    return -1;
  else if(pid != 0)
    exit(0);
 
  /// become session leader
  if(setsid() < 0)
    return -1;
  
  /// clear file mode creation mask
  umask(0);
  
  /// change the default dir to "/"
  if(chdir(RUN_DIR)!=0)
    return -1;
  
  return 0;
}