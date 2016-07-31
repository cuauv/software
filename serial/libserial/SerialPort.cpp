#include "SerialPort.h"

#include <system_error>
#include <algorithm>

#include <glob.h>
#include <termios.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

namespace cuauv {
namespace serial {

class io_error : public std::system_error {
    private:
        std::string m_msg;
    public:
    explicit io_error(const std::string& op) : std::system_error(errno, std::system_category()),
            m_msg(op + ": " + code().message())
    {
    }

    const char* what() const noexcept override {
        return m_msg.c_str();
    }
};

SerialPort::SerialPort(const std::string& path) : UnixFilePort(getFd(path), path)
{
}

int SerialPort::getFd(const std::string& path) {
    int fd;
    if ((fd = open(path.c_str(), O_RDWR | O_NOCTTY | O_SYNC)) < 0) {
        throw io_error("Failed to open serial port " + path);
    }

    struct termios tty;
    memset(&tty, 0, sizeof(tty));
    if (tcgetattr(fd, &tty) != 0) {
        throw io_error(path + " doesn't look like a serial port");
    }

    cfmakeraw(&tty);

    if (cfsetospeed(&tty, B57600) != 0) {
        throw io_error("Failed to set output baud rate for " + path);
    }

    if (cfsetispeed(&tty, B57600) != 0) {
        throw io_error("Failed to set input baud rate for " + path);
    }

    //tty.c_iflag = IGNPAR;  // ignore parity errors

    //tty.c_lflag = 0;                // no signaling chars, no echo,
    //                                // no canonical processing
    //tty.c_oflag = 0;                // no remapping, no delays
    tty.c_cc[VMIN]  = 0;            // read doesn't block
    tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

    //tty.c_cflag = CLOCAL | CREAD | CS8; // Ignore status lines, enable read, 8 bits per byte

    if (tcsetattr(fd, TCSANOW, &tty) != 0) {
        throw io_error("Failed to configure serial port " + path);
    }

    tcflush(fd, TCIFLUSH);

    return fd;
}

void SerialPort::flush() {
    tcflush(m_fd_tx, TCIOFLUSH);
}

void SerialPort::write(std::vector<uint8_t> data) {
    size_t offset = 0;
    while (offset < data.size()) {
        auto chunksize = std::min((size_t) 32, data.size() - offset);
        std::vector<uint8_t> chunk(data.begin() + offset, data.begin() + offset + chunksize);
        UnixFilePort::write(chunk);
        tcdrain(m_fd_tx);
        offset += chunksize;
    }
}

std::set<std::string> SerialPort::listPorts() {
    glob_t globbuf;
    glob("/dev/ttyUSB*", GLOB_NOSORT, nullptr, &globbuf);
    glob("/dev/ttyACM*", GLOB_NOSORT | GLOB_APPEND, nullptr, &globbuf);

    std::set<std::string> ports;
    for (unsigned int i = 0; i < globbuf.gl_pathc; i++) {
        ports.emplace(globbuf.gl_pathv[i]);
    }

    globfree(&globbuf);

    return ports;
}


}} // end namespace cuauv::serial
