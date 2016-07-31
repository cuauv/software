#include <iostream>
#include <unistd.h>
#include "auvlog/client.h"

int main () {

  auvlog_init();
  sleep(1); // Note that it can take a small amount of time for the socket to connect.
  auvlog_log("Example.First", "A logging message!");
  std::cout << "Here!" << std::endl;
  auvlog_log("Example.Second", "Another logging message!");
  
  while (1) {
    sleep(1);
    auvlog_log("Example.Contd", "Useful messages continue...");
  };
 
};
