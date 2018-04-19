#include "client.h"

nn::socket auvlog_sock ( AF_SP, NN_PUB );

void auvlog_log_raw(std::string tree, double timestamp, std::string message, std::string filename, int lineno, std::string block, std::string linetxt) {
  json e = {
    {"tree", tree},
    {"timestamp", timestamp},
    {"message", message},
    {"filename", filename}, 
    {"lineno", lineno},
    {"block", block},
    {"linetxt", linetxt}
  };
  std::string ser = e.dump();
  auvlog_sock.send(ser.c_str(), ser.size(), 0);
};
