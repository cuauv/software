#include "utils.h"

#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

//Returns directory of executable calling it (Linux magic)
//NOTE: includes ending slash
std::string getBinDir() {
  char buf[1024];
  int len = readlink("/proc/self/exe", buf, sizeof(buf)-1);
  buf[len] = '\0';
  std::string bin_filename(buf);
  size_t last_slash = bin_filename.find_last_of('/');
  return bin_filename.substr(0, last_slash+1);
}

//test whether a file is opened.
bool file_opened(char *filename) {
  pid_t child_pid = fork();
  if (!child_pid) {
    if (!freopen("/dev/null", "w", stdout)) {
      fprintf(stderr, "Could not redirect stdout to /dev/null\n");
    }
    if (!freopen("/dev/null", "w", stderr)) {
      fprintf(stderr, "Could not redirect stderr to /dev/null\n");
    }

    execlp("fuser", "fuser", "-s", filename, NULL);
    exit(-1);
  }
  int status;
  waitpid(child_pid, &status, 0);
  status = WEXITSTATUS(status);
  return status == 0;
}

int socket_connect(int &sockfd, const char *addr, int port) {
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    return -1;
  }

  struct sockaddr_in serv_addr;
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(port);
  serv_addr.sin_addr.s_addr = inet_addr(addr);

  if (connect(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr))) {
    return -2;
  }

  return 0;
}
