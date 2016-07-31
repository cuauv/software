#ifndef SHM_SOCKET_H_
#define SHM_SOCKET_H_

#include "libshm/c/serialize.h"
#include "libshm/c/watcher.h"

#include <stdint.h>
#include <string>
#include <unistd.h>
#include <vector>

#define MAX_FIELDS 70

class Serialize {
  public:
    Serialize(int fd);
    virtual ~Serialize() {
        Close();
    }

    // Read until the socket closes.
    void Handle(bool rebroadcast);

    // Send a complete copy of shared memory to the client.
    // TODO: only send to the new client instead of all clients.
    void WriteEverything() {
        WriteChanged(true);
    }

    // Poll shared memory for changes and write them to all sockets.
    // The argument is ignored and the function never returns.
    static void* Serve(void*);

  private:
    bool Close();

    bool ReadOne(bool rebroadcast);
    bool Write(uint32_t n);

    // Write 1 group to all clients.
    // Assumes client list is locked.
    static void WriteToAll(uint32_t n);

    // Find all changed groups and call WriteToAll on them.
    // Sends all variables instead of altered ones if flag is set.
    static void WriteChanged(bool everything);

    bool Recv(size_t len, void* buf);
    bool Send(size_t len, void* buf);

    void LoadGroups();

    static watcher_t WatchAll();

    static void ClearStream(); // sets all groups' stream value to 0

    bool closed;
    int fd;
};

#endif  // SHM_SOCKET_H_
