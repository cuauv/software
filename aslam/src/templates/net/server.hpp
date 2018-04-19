#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <thread>

#include "../math/common.hpp"
#include "../math/fastslam.h"

void spawn_server(Map& map) {
  int socket_desc, client_sock, c;
  struct sockaddr_in server, client;

  socket_desc = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_desc == -1)
    throw s::runtime_error("Failed to create socket!");

  // http://stackoverflow.com/questions/24194961/how-do-i-use-setsockoptso-reuseaddr
  int _tmp = 1;
  if (setsockopt(socket_desc, SOL_SOCKET, SO_REUSEADDR, &_tmp, sizeof(int)) < 0)
    throw s::runtime_error("Failed to set SO_REUSEADDR");

  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons ( 8888 );
  
  if (bind(socket_desc, (struct sockaddr *) &server, sizeof(server)) < 0)
    throw s::runtime_error("Failed to bind socket to port 8888!");
  
  listen(socket_desc, 3);

  c = sizeof(struct sockaddr_in);

  while ( (client_sock = accept(socket_desc, (struct sockaddr *) &client, (socklen_t*) &c)) ) {
    AUVLOG("Got a client!")
    auto func = [&]() {
      uint8_t message [BYTES_PER_MESSAGE($!len(objects)!$)];
      unsigned int num = NUM_COMBINED_STATE_PARTICLES * NUM_PARTICLES_PER_OBJECT_MAP * $!len(objects)!$;
      memcpy(&message[0], &num, sizeof(unsigned int));
      if (send(client_sock, message, sizeof(unsigned int), MSG_NOSIGNAL) < 4) {
        AUVLOG("Error writing initial point count, client issues?")
        return;
      };
      for(;;) {
        int ptr = 0;
<!--(for o in objects)-->
        {
          message[ptr] = (uint8_t) 65;
          ptr++;
          float tmp;
  <!--(for j in range(3))-->
          {
            tmp = static_cast<float>($!o['color'][j]!$);
            memcpy(&message[ptr], &tmp, sizeof(float));
            ptr += sizeof(float);
          }
  <!--(end)-->
          auto pc = PointCloud<$!o['state_type']!$, $!o['covariance_type']!$> ( map.combined_$!o['name']!$ ); // do we need to copy this?
          for (unsigned int i = 0; i < pc.particles.size(); i++) {
            message[ptr] = (uint8_t) 64;
            ptr++;
  <!--(for d in range(3))-->
            // TODO: Should be dimensionality
            tmp = static_cast<float>(pc.particles[i].first[$!d!$]);
            memcpy(&message[ptr], &tmp, sizeof(float));
            ptr += sizeof(float);
  <!--(end)--> 
            tmp = static_cast<float>(pc.particles[i].second);
            memcpy(&message[ptr], &tmp, sizeof(float));
            ptr += sizeof(float);
          }
        }
<!--(end)-->
        uint8_t client_msg[1];
        if (recv(client_sock, client_msg, 1, 0) < 1) {
          AUVLOG("Error reading socket, client disconnected?")
          break;
        };
        if (send(client_sock, message, BYTES_PER_MESSAGE($!len(objects)!$), MSG_NOSIGNAL) < BYTES_PER_MESSAGE($!len(objects)!$)) {
          AUVLOG("Error writing socket, client disconnected?")
          break;
        };
        s::this_thread::sleep_for(s::chrono::milliseconds(100));
      };
      AUVLOG("Killing client thread")
    };
    s::thread client_thread (func);
    client_thread.detach();
  }

  if (client_sock < 0)
    throw s::runtime_error("Socket accept failed!");
};

// point - X, Y, Z, W (4 * 4 = 16)
// color - R, G, B (4 * 3 = 12)
// point - 0 (1 byte)
// color - 1 (1 byte)
