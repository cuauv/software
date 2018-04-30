#include "serial.h"

#include <sys/wait.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <exception>

using namespace sensorserial;

SerialPort::SerialPort(const char* devname, int baud_rate) {
    this->devname = devname;
    this->baud_rate = baud_rate;
    opened = false;
    openPort();
}

bool SerialPort::isOpen() {
    return opened;
}

int SerialPort::openPort() {
    int ret;
    //fd = open(devname, O_RDWR | O_NOCTTY | O_NONBLOCK | O_SYNC);
    fd = open (devname, O_RDWR | O_NOCTTY | O_SYNC);

    if (fd == -1) {
        printf("Unable to open device %s\n", devname);
        perror(devname);
        //throw std::exception();
        return -1;
    } else {
        struct termios term;

        tcgetattr(fd, &term);

        term.c_cflag = CS8 | CLOCAL | CREAD | HUPCL;
        term.c_iflag = IGNPAR;
        term.c_oflag = 0;
        term.c_lflag = 0;

        term.c_cc[VMIN] = 0;
        term.c_cc[VTIME] = 10;
   
        int baudconst = 0;
        switch (baud_rate) {
            case 4800:
                baudconst = B4800;
                break;
            case 9600:
                baudconst = B9600;
                break;
            case 19200:
                baudconst = B19200;
                break;
            case 38400:
                baudconst = B38400;
                break;
            case 57600:
                baudconst = B57600;
                break;
            case 115200:
                baudconst = B115200;
                break;
            case 230400:
                baudconst = B230400;
                break;
            #ifdef B460800
            case 460680:
                baudconst = B460800;
            #endif    
            #ifdef B500000
            case 500000:
                baudconst = B500000;
                break;
            #endif
            #ifdef B576000
            case 576000:
                baudconst = B576000;
                break;
            #endif
            #ifdef B921600
            case 921600:
                baudconst = B921600;
                break;
            #endif
            #ifdef B1000000
            case 1000000:
                baudconst = B1000000;
                break;
            #endif
            #ifdef B1152000
            case 1152000:
                baudconst = B1152000;
                break;
            #endif
            default:
            #ifdef B1152000
                baudconst = B1152000;
            #else
                baudconst = B230400;
            #endif
        }

        if (cfsetispeed(&term, baudconst) != 0) {
            printf("Error setting input speed\n");
        }
        if (cfsetospeed(&term, baudconst) != 0) {
            printf("Error setting output speed\n");
        }
    
        if ((ret = tcsetattr(fd, TCSANOW, &term)) != 0) {
            printf ("Error with tcsetattr: %d, errno: %d\n", ret, errno);
        }
    
        if ((ret = tcflush(fd, TCIFLUSH)) != 0) {
            printf ("Error with tcflush: %d, errno: %d\n", ret, errno);
        }

        struct termios tios;
        
        tcgetattr(fd,&tios);

        if (tios.c_cflag & CSTOPB) {
            printf("CSTOPB set\n");
        }
    }
    if (!ret) {
        opened = true;
    }
    return ret;
}

SerialPort::~SerialPort() {
    close(fd);
    fd = -1;
}

//try writing MAX_RETRIES times
int SerialPort::writeSer(const unsigned char* buf, ssize_t size) {
    u_int8_t count = 0;
    ssize_t written;
    ssize_t totalWritten = 0;
   
    unsigned char *ucharbuf;
    
    while (size > 0) {
//        usleep(10000);
        written = write(fd, buf, 1);
        
        if (written == -1) {
            printf("Errno: %d\n", errno);
            if (!(fcntl(fd, F_GETFL) != -1 || errno != EBADF)) {
                fprintf(stderr, "Port %s no longer open! allegedly\n", devname);
                fprintf(stderr, "Attempting to reopen file descriptor\n");
                fprintf(stderr, "Entering loop to reopen port...\n");
                while (openPort())
                    usleep(50000);

                fprintf(stderr, "Port %s should now be open\n", devname);

            }
            else {
                fprintf(stderr, "Can't write but we think port %s is open!!!\n", devname);
                fprintf(stderr, "This is bad...\n");
                fprintf(stderr, "Let's close it and try to reopen\n");
                close(fd);
                while (openPort())
                    usleep(50000);
                fprintf(stderr, "Port %s should now be open\n", devname);
            }
            count++;
            continue;
        }
        totalWritten += written;

//        printf("Bits written: %d  ",written);
        size -= written;
        ucharbuf = (unsigned char*)buf;
        ucharbuf += written;
        buf = ucharbuf;

        /*if (count++ >= MAX_RETRIES) {
            //printf("Write failure\n");
            return -1;
        }*/
    }
    return totalWritten;
    
}

int SerialPort::sendBreak(int duration) {
    if (fd == -1) {
        return -1;
    }
    return tcsendbreak(fd, duration);
}

ssize_t SerialPort::readWithTimeout(unsigned char *buf, size_t maxSize, long uwait)
{
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

    if(uwait < 0)
        return -1;
    int rc;
    fd_set fds;
    struct timeval tv;
    //int nRead = 0;
    tv.tv_sec = 0;
    tv.tv_usec = uwait;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);

    //now wait for bytes
    rc=select(fd+1, &fds, NULL, NULL, &tv);
    if(rc<0)
        return -1;
    if(FD_ISSET(fd, &fds) == 0)
        return 0;
    return read(fd,buf,maxSize);
}

ssize_t SerialPort::readnWithTimeout(unsigned char *buf, size_t toRead, long uwait)
{
    //check to see if serial port isn't working out
    if (fd == -1) {
        printf("SAD 4\n");
        return -1;
    }

    //note on my logic: no special handling for uwait = 0.  readn is meaningless in that case

    if(uwait < 0){
        printf("SAD 3\n");
        return -1;
    }
   
    ssize_t nRead = 0;
    ssize_t ret;
    struct timeval tv;
    if(gettimeofday(&tv, NULL) != 0) {
	printf("Timeofday\n");
        return -1; 
    }
    long current_time = tv.tv_usec + tv.tv_sec * 1000000;
    long end_time = current_time + uwait;

    while(current_time <= end_time && nRead < (ssize_t)toRead)
    {
        ret = readWithTimeout(buf + nRead, toRead - nRead, end_time - current_time);
        if(ret < 0){ 
            printf("SAD 1\n");
            return -1;
	}
        nRead += ret;
        if(gettimeofday(&tv, NULL) != 0) {
            printf("SAD 2\n");
            return -1;
	}
        current_time = tv.tv_usec + tv.tv_sec * 1000000;
    }

    printf("%ld\n", nRead);
    return nRead;
}

ssize_t SerialPort::getBytesWaiting() {
    int bytes_available;
    ioctl(fd, FIONREAD, &bytes_available);
    return bytes_available;
}

int SerialPort::flushBuffers()
{
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

    return tcflush(fd, TCIOFLUSH);
}

int SerialPort::readSer(unsigned char* buf, size_t size) {
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

    u_int8_t tmpBuf;
    int ret = 0;
    size_t i;
   
    unsigned char * ucharbuf;

    for (i = 0; i < size;) {
//        (10000);
        ret = read(fd, &tmpBuf, 1);
        if (ret > 0) {
            *((u_int8_t*)(buf)) = tmpBuf;
            ucharbuf = (unsigned char*)buf;  // this is a hack (mine, for gcc4) and I don't like it -Tommy
                        // why were we using void*s in the first place anyway?
        ucharbuf++;
        buf = ucharbuf;
            i++;
        }
        if (errno != 0) {
            perror(this->devname);
        }

    }
    return i;
}
