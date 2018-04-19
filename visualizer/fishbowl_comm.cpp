#include "fishbowl_comm.h"

#include <cstdio>
#include <cstdlib>

#include <sys/socket.h>
#include <string.h> // for memcpy
#include <unistd.h>

#include "fishbowl/bits.hpp"
#include "misc/utils.h"

using cuauv::fishbowl::uint32_of_bytes;
using cuauv::fishbowl::float64_of_bytes;
using cuauv::fishbowl::bytes_of_uint32;

namespace fishbowl_comm {

enum class request_type {
    REMOVE = 0,
    ELIST = 1,
    EADD = 2,
    ESET = 3,
    EGET = 4,
    OCLASSES = 5,
    OLIST = 6,
    OADD = 7,
    OSET = 8,
    CLIST = 9,
    CADD = 10,
    CSET = 11,
    CVIEW = 12,
    TADD = 13,
    ESTREAM = 14,
    CSTREAM = 15,
    CBCAST = 16,
    CONFIG = 17,
    OGET = 18,
    EXSET = 19,
    EXGET = 20,
    EXSTREAM = 21,
    REASSURE = 33,
    RUN = 254,
    PAUSE = 255,
};

enum class response_type {
    OK = 0,
    ERROR = 1,
    ADDED = 2,
    ELISTED = 3,
    EGOT = 4,
    OLISTED = 5,
    OGOT = 6,
    CGOT = 7,
    CONFIGED = 8,
    EXGOT = 9,
    TRIGGERED = 128,
    EDATA = 129,
    CDATA = 130,
    UNTRIGGERED = 131,
    PSTART = 132,
    PFAIL = 133,
    PEND = 134,
    EXDATA = 135,
    SPIED = 136,
    COMPLETED = 255,
};

struct fishbowl_msg {
  uint8_t type;
  uint8_t length[4];

  union {

    struct eget {
      uint8_t id[4];
      uint8_t attr;
    } eget;

    struct egot {
      uint8_t attr;
      uint8_t data[0];
    } egot;

    //struct exset {
    //    uint8_t id[4];
    //    uint8_t key_len;
    //    byte* key;
    //    uint32 data_len;
    //    byte* data;
    //};

    struct exget {
      uint8_t id[4];
      uint8_t key_len;
      uint8_t key[100];
    } exget;

    struct exgot {
      uint8_t key_len;
      uint8_t key[10];      // Enough to appease compiler. TODO Make better.
      uint8_t data_len[4];
      uint8_t data[1];
    } exgot;

    struct elisted {
      uint8_t n[4];
      uint8_t ids[0];
    } elisted;

  } body;
};

int transaction(int sockfd, void *msg, int msg_size, uint8_t *reply) {
  if (send(sockfd, msg, msg_size, MSG_NOSIGNAL) < 0) {
    fprintf(stderr, "ERROR send failed\n");
    return -1;
  }

  uint32_t total_bytes = 0;
  auto *formatted_reply = (struct fishbowl_msg *)reply;
  while (1) {
    int bytes_recvd = recv(sockfd, &reply[total_bytes], 100, 0);
    if (bytes_recvd <= 0) {
      fprintf(stderr, "ERROR recv failed\n");
      return -1;
    }
    total_bytes += bytes_recvd;

    uint32_t length = uint32_of_bytes(formatted_reply->length);
    if (total_bytes >= length) {
      break;
    }

    if (length > 90) {
      fprintf(stderr, "Messages over 90 not supported; please fix me!\n");
      break;
    }
  }

  return total_bytes;
}

int sockfd;
int connect() {
  return socket_connect(sockfd, "127.0.0.1", 7772);
}

void disconnect() {
  close(sockfd);
}

struct fishbowl_msg msg;
uint8_t reply[100];
auto *formatted_reply = (struct fishbowl_msg *)reply;

void fill_fishbowl_eget(uint32_t entity, int attr_id) {
  msg.type = (uint8_t)request_type::EGET;
  bytes_of_uint32(msg.length, 5);
  bytes_of_uint32(msg.body.eget.id, entity);
  msg.body.eget.attr = attr_id;
}

std::vector<uint32_t> get_all_entities() {
  msg.type = (uint8_t)request_type::ELIST;
  bytes_of_uint32(msg.length, 0);

  std::vector<uint32_t> entity_ids;

  if (transaction(sockfd, &msg, 5, reply) <= 0) {
    fprintf(stderr, "Transaction failed\n");
    // TODO Better way of passing failure.
    return entity_ids;
  }

  int entities = uint32_of_bytes(formatted_reply->body.elisted.n);
  for (int i = 0; i < entities; i++) {
    entity_ids.push_back(uint32_of_bytes(&formatted_reply->body.elisted.ids[4*i]));
  }

  return std::move(entity_ids);
}

int get_position(uint32_t entity, double &x, double &y, double &z) {
  fill_fishbowl_eget(entity, ATTR_ID_X);

  if (transaction(sockfd, &msg, 10, reply) <= 0) {
    fprintf(stderr, "Transaction failed\n");
    return -1;
  }

  x = float64_of_bytes(&formatted_reply->body.egot.data[0]);
  y = float64_of_bytes(&formatted_reply->body.egot.data[8]);
  z = float64_of_bytes(&formatted_reply->body.egot.data[16]);
  return 0;
}

int get_orientation(uint32_t entity, double &q0, double &q1, double &q2, double &q3) {
  fill_fishbowl_eget(entity, ATTR_ID_Q);

  if (transaction(sockfd, &msg, 10, reply) <= 0) {
    fprintf(stderr, "Transaction failed\n");
    return -1;
  }

  q0 = float64_of_bytes(&formatted_reply->body.egot.data[0]);
  q1 = float64_of_bytes(&formatted_reply->body.egot.data[8]);
  q2 = float64_of_bytes(&formatted_reply->body.egot.data[16]);
  q3 = float64_of_bytes(&formatted_reply->body.egot.data[24]);
  return 0;
}

int get_xattr(uint32_t entity, std::string key, std::string &data) {
  if (key.size() > 90) {
    fprintf(stderr, "Keys longer than 90 not supported; please fix me!\n");
    return -1;
  }

  msg.type = (uint8_t)request_type::EXGET;
  bytes_of_uint32(msg.length, 5 + key.size());
  bytes_of_uint32(msg.body.exget.id, entity);
  msg.body.exget.key_len = key.size();
  memcpy(msg.body.exget.key, key.data(), key.size());

  if (transaction(sockfd, &msg, 10 + key.size(), reply) <= 0) {
    fprintf(stderr, "Transaction failed\n");
    return -2;
  }

  if (formatted_reply->type != (uint8_t)response_type::EXGOT) {
    return -3;
  }

  uint8_t key_len = formatted_reply->body.exgot.key_len;

  std::string key_s((const char *)formatted_reply->body.exgot.key, key_len);
  if (key_s != key) {
    fprintf(stderr, "Returned key (%s) does not match requested (%s) for entity with id %d\n", key_s.c_str(), key.c_str(), entity);
    return -4;
  }

  uint32_t data_len = uint32_of_bytes(formatted_reply->body.exgot.key + key_len);
  data = std::string((const char *)(formatted_reply->body.exgot.key + key_len + 4), data_len);

  return 0;
}

}
