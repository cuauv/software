#include <string.h>
#include <thread>
#include <chrono>
#include <iostream>
#include <nanomsg/pubsub.h>
#include "json.hpp"
#include "nn.hpp"

using json = nlohmann::json;

extern nn::socket auvlog_sock;

void auvlog_log_raw(std::string tree, double timestamp, std::string message, std::string filename, int lineno, std::string block, std::string linetxt);
  
// If anyone can figure out how to get the C preprocessor to read files at compile time, 
// add the text of the current line as the last argument.

#define auvlog_log(tree, message) auvlog_log_raw(tree, std::time(0), message, __FILE__, __LINE__, __func__, "")

#define auvlog_log_stdout(tree, message)\
   auvlog_log_raw(tree, std::time(0), message, __FILE__, __LINE__, __func__, ""); \
   std::cout \
    << "\033[1;31m" << std::time(0) << "\033[0m " \
    << "\033[0;34m" << tree << "\033[0m " \
    << "\033[0;33m" << __FILE__ << ":" << __func__ << "@" << __LINE__ << "\033[0m " \
    << message << std::endl;

#define auvlog_init() auvlog_sock.connect("tcp://127.0.0.1:7654"); std::this_thread::sleep_for(std::chrono::milliseconds(100));

/* Why this needs to be macroed is an excellent question; see SOF-66.
   The sleep gives the socket enough time to connect; prevents initial messages from getting dropped. */
