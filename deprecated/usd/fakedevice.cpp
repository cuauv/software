#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fcntl.h>
#include <errno.h>
#include <iomanip>
#include <iterator>
#include <netinet/in.h>
#include <popt.h>
#include <pthread.h>
#include <sstream>
#include <unistd.h>
#include <vector>

#include "protocol.h"

namespace {

// Since the address field is 16 bits, there could be 65536 registers.
const int DEFAULT_MAX_VALID = 128;
const int MAX_REGISTERS = 65536;
bool readOnly[MAX_REGISTERS];

struct registers_t {
    union {
        uint8_t u8[MAX_REGISTERS * 2];
        uint16_t u16[MAX_REGISTERS];
        uint32_t u32[MAX_REGISTERS / 2];
        float f32[MAX_REGISTERS / 2];
    };
} registers;

int maxReg = DEFAULT_MAX_VALID;

}

// Prototypes, blah blah...
bool parseCommand(const std::string& command);
void* serialReadLoop(void* arg);
void respond(int tty, uint8_t type, uint8_t count, uint16_t address);
bool isValidPacket(uint8_t function);
bool hasPayload(uint8_t function);
uint16_t swap16(uint16_t x);

using namespace std;

int main(int argc, const char** argv) {

    int readOnlyReg;

    // Damn, can't use POPT_ARG_SHORT, because it's signed!
    struct poptOption opts[] = {
        {"num-valid", 'n', POPT_ARG_INT, &maxReg, 0,
            "number of valid registers"},
        {"read-only", 'r', POPT_ARG_INT, &readOnlyReg, 'r',
            "specifies a read-only register"},
        POPT_AUTOHELP
        POPT_TABLEEND
    };

    poptContext context = poptGetContext(NULL, argc, argv, opts, 0);

    int rc;
    while ((rc = poptGetNextOpt(context)) > 0) {
        // Honestly this could only be 'r', so I'll just do that.
        if (readOnlyReg < 0 || readOnlyReg >= MAX_REGISTERS) {
            cerr << readOnlyReg << " is not a valid register" << endl;
            return 2;
        }
        readOnly[readOnlyReg] = true;
    }

    if (rc < -1) {
        cerr << poptStrerror(rc) << " for " << poptBadOption(context, 0) << endl;
        return 2;
    }

    if (maxReg < 0 || maxReg >= MAX_REGISTERS) {
        cerr << maxReg << " is not a valid number of registers" << endl;
        return 2;
    }

    poptFreeContext(context);

    int master = posix_openpt(O_RDWR);
    if (master < 0) {
        cerr << "Error " << errno << " on posix_openpt()\n" << endl;
        return 1;
    }

    if (grantpt(master) != 0) {
        cerr << "Error " << errno << " on grantpt()\n" << endl;
        return 1;
    }

    if (unlockpt(master) != 0) {
        cerr << "Error " << errno << " on unlockpt()\n" << endl;
        return 1;
    }

    cout << "Device ready to receive data on " << ptsname(master) << endl;

    // Fake device's ID is 0x1234 (4660) because I said so.
    registers.u16[ID_ADDRESS] = swap16(0x1234);
    readOnly[ID_ADDRESS] = true;

    pthread_t serialReadThread;
    pthread_create(&serialReadThread, NULL, serialReadLoop, &master);

    string input;
    bool keepGoing = true;
    while (keepGoing) {
        getline(cin, input);
        keepGoing = parseCommand(input);
    }

    return 0;
}

bool parseCommand(const string& command) {
    if (command == "exit") {
        return false;
    }

    // Tokenize by whitespace
    istringstream buffer(command);
    vector<string> tokens;
    copy(istream_iterator<string>(buffer), istream_iterator<string>(),
         std::back_inserter(tokens));

    if (tokens.size() == 0) {
        cout << "Invalid command" << endl;
        return true;
    }

    if (tokens[0] == "format") {
        for (int i = 0; i < maxReg; ++i) {
            if (i % 16 == 0) {
                cout << setw(5) << i << "|";
            }
            cout << setw(5) << registers.u16[i] << " ";
            if (i % 16 == 15) {
                cout << "| " << i << endl;
            }
        }
        return true;
    }

    if (tokens[0] == "print") {
        int reg;
        if (tokens.size() == 1) {
            cout << "command <print> requires an argument" << endl;
        }
        else {
            stringstream(tokens[1]) >> reg;
            cout << registers.u16[reg] << endl;
        }
        return true;
    }

    cout << "You entered: " << command << endl;
    return true;
}

void* serialReadLoop(void* arg) {
    int master = *reinterpret_cast<int*>(arg);
    while (true) {
        uint8_t receive[MAX_PACKET_LENGTH];
        uint8_t status = 0;

        // Read anything the computer sent this "device".
        ssize_t bytes_read = read(master, receive, BASE_PACKET_LENGTH);

        if (bytes_read < 0) {
            cerr << "Error when reading header! errno is " << errno << endl;
            // TODO: Soemething more intelligent?
            continue;
        }

        uint8_t packetType = receive[0];
        uint8_t count = receive[1];
        uint16_t address = ntohs(*(reinterpret_cast<uint16_t*>(receive + 2)));

        // Invalid function: send back an error packet.
        if (!isValidPacket(packetType)) {
            cerr << "Invalid function " << (int) packetType << endl;
            status = PACKET_TYPE_INVALID_FUNCTION;
        }

        // Invalid register: send back an error packet.
        else if (address + count >= maxReg) {
            cerr << "Invalid register " << address << endl;
            status = PACKET_TYPE_INVALID_REGISTER;
        }

        // Readonly registers: send back an error packet if not reading.
        else if (packetType != PACKET_TYPE_READ) {
            // Need to check all registers this request touches.
            for (int i = 0; i < count; ++i) {
                if (readOnly[address + i]) {
                    cerr << "Readonly register " << (address + i) << endl;
                    status = PACKET_TYPE_READONLY_REGISTER;
                    break;
                }
            }
        }

        // Does packet have payload?
        int size = BASE_PACKET_LENGTH;
        if (hasPayload(packetType)) {
            // Read said payload into buffer:
            ssize_t bytes_read = read(master, receive + size, 2 * count);
            if (bytes_read < 0) {
                cerr << "Error when reading payload! errno is " << errno << endl;
                // TODO: Something more intelligent?
                continue;
            }
            cout << "Adding to size" << endl;
            size += 2 * count;
        }

        // Read checksum and append to end of buffer:
        bytes_read = read(master, receive + size, 1);
        if (bytes_read < 0) {
            cerr << "Error when reading checksum! errno is " << errno << endl;
            // TODO: Something more intelligent?
            continue;
        }

        uint8_t checksumReceived = receive[size];
        uint8_t checksumExpected = computeChecksum(size, receive);

        if (status == 0 && checksumReceived != checksumExpected) {
            cerr << "Bad checksum " << (int) checksumReceived << ", expected "
                << (int) checksumExpected << endl;
            status = PACKET_TYPE_BAD_CHECKSUM;
        }

        if (status != 0) {
            respond(master, status, count, address);
            continue;
        }

        // TODO: Timeout? For invalid length...

        int responseSize;
        uint8_t send[MAX_PACKET_LENGTH];
        ssize_t bytes_written;

        switch (packetType) {
        case PACKET_TYPE_READ:
            send[0] = packetType | PACKET_TYPE_RESPONSE;
            send[1] = count;
            *(reinterpret_cast<uint16_t*>(send + 2)) = htons(address);

            // Copy registers into send buffer:
            memcpy(send + 4, registers.u8 + address * 2, count * 2);

            responseSize = BASE_PACKET_LENGTH + count * 2;
            send[responseSize] = computeChecksum(responseSize, send);

            bytes_written = write(master, send, responseSize + 1);
            if (bytes_written < 0) {
                cerr << "Error sending read response! errno " << errno << endl;
            }
            break;
        case PACKET_TYPE_WRITE:
            // Copy send buffer into registers
            memcpy(registers.u8 + address * 2, receive + 4, count * 2);

            respond(master, packetType | PACKET_TYPE_RESPONSE, count, address);
            break;
        case PACKET_TYPE_AND:
            for (int i = 0, j = address * 2; i < count * 2; ++i, ++j) {
                registers.u8[j] &= receive[4 + i];
            }

            respond(master, packetType | PACKET_TYPE_RESPONSE, count, address);
            break;
        case PACKET_TYPE_OR:
            for (int i = 0, j = address * 2; i < count * 2; ++i, ++j) {
                registers.u8[j] |= receive[4 + i];
            }

            respond(master, packetType | PACKET_TYPE_RESPONSE, count, address);
            break;
        case PACKET_TYPE_XOR:
            for (int i = 0, j = address * 2; i < count * 2; ++i, ++j) {
                registers.u8[j] ^= receive[4 + i];
            }

            respond(master, packetType | PACKET_TYPE_RESPONSE, count, address);
            break;
        case PACKET_TYPE_RESTART:
            cerr << "Uh... let's just pretend I restarted, okay?" << endl;
            break;
        default:
            cerr << "There's no way I could have gotten here... " << endl;
        }
    }
    return NULL;
}

void respond(int tty, uint8_t type, uint8_t count, uint16_t address) {
    uint8_t send[BASE_PACKET_LENGTH + 1];

    send[0] = type;
    send[1] = count;
    *(reinterpret_cast<uint16_t*>(send + 2)) = htons(address);
    send[4] = computeChecksum(4, send);

    ssize_t bytes_written = write(tty, send, 5);
    if (bytes_written < 0) {
        cerr << "Error sending " << type << ", errno" << errno << endl;
    }
}

bool isValidPacket(uint8_t function) {
    switch (function) {
    case PACKET_TYPE_NOP:
    case PACKET_TYPE_READ:
    case PACKET_TYPE_WRITE:
    case PACKET_TYPE_AND:
    case PACKET_TYPE_OR:
    case PACKET_TYPE_XOR:
    case PACKET_TYPE_RESTART:
        return true;
    default:
        return false;
    }

    // Uhm... just in case?
    return false;
}

bool hasPayload(uint8_t function) {
    switch (function) {
    case PACKET_TYPE_WRITE:
    case PACKET_TYPE_AND:
    case PACKET_TYPE_OR:
    case PACKET_TYPE_XOR:
        return true;
    default:
        return false;
    }

    // Uhm... just in case?
    return false;
}

uint16_t swap16(uint16_t x) {
    return (x << 8) | (x >> 8);
}
