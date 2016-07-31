#include <string>

//Returns directory of executable calling it (Linux magic)
//NOTE: includes ending slash
std::string getBinDir();
bool file_opened(char *filename);

// Connects to a server running at addr on port using a TCP/IP socket.
int socket_connect(int &sockfd, const char *addr, int port);
