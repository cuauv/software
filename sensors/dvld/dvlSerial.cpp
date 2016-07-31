#include "dvlSerial.h"

#include <sys/wait.h>
#include <sys/time.h>
#include <errno.h>

using namespace sensord;

DVLSerialPort::DVLSerialPort(const char* devname) {
    int ret;
    //fd = open(devname, O_RDWR | O_NOCTTY | O_NONBLOCK | O_SYNC);
    fd = open (devname, O_RDWR | O_NOCTTY | O_SYNC);

    if (fd == -1) {
        printf("Unable to open device %s\n", devname);
        printf("Errno gives:%i\n", errno);
    } else {
        struct termios term;
    
        tcgetattr(fd, &term);

        //based on code from dvld
        //reportedly from RDI
        term.c_cflag = CS8 | B9600 | CLOCAL | HUPCL | CREAD | CSTOPB;
        term.c_iflag = IGNPAR | IGNBRK;
        term.c_oflag = 0;
        term.c_lflag = 0;

        term.c_cc[VTIME]=10;
        term.c_cc[VMIN] = 0;
    
        if ((ret = tcsetattr(fd, TCSANOW, &term)) != 0) {
            printf ("Error with tcsetattr: %d, errno: %d\n", ret, errno);
        }
    
        if ((ret = tcflush(fd, TCIFLUSH)) != 0) {
            printf ("Error with tcflush: %d, errno: %d\n", ret, errno);
        }    
    }
}

DVLSerialPort::~DVLSerialPort() {
    close(fd);
    fd = -1;
    printf("Hey! I got called!\n");
}

//try writing MAX_RETRIES times
int DVLSerialPort::writeSer(const unsigned char* buf, ssize_t size) {
    u_int8_t count = 0;
    ssize_t written;
    ssize_t totalWritten = 0;
   
    unsigned char *ucharbuf;
    
    while (size > 0) {
//        usleep(10000);
        written = write(fd, buf, 1);
        
        if (written == -1) {
        //    printf("Errno: %d\n", errno);
        }
        totalWritten += written;

//        printf("Bits written: %d  ",written);
        size -= written;
        ucharbuf = (unsigned char*)buf;
	ucharbuf += written;
	buf = ucharbuf;

        if (count++ >= MAX_RETRIES) {
            //printf("Write failure\n");
            return -1;
        }
    }
    return totalWritten;
    
}

int DVLSerialPort::sendBreak(int duration) {
    if (fd == -1) {
        return -1;
    }
    return tcsendbreak(fd, duration);
}

ssize_t DVLSerialPort::readWithTimeout(unsigned char *buf, size_t maxSize, long uwait)
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

ssize_t DVLSerialPort::readnWithTimeout(unsigned char *buf, size_t toRead, long uwait)
{
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

	//note on my logic: no special handling for uwait = 0.  readn is meaningless in that case
	
	if(uwait < 0)
		return -1;
	
	ssize_t nRead = 0;
	ssize_t ret;
	struct timeval tv;
	if(gettimeofday(&tv, NULL) != 0)
		return -1;
	long current_time = tv.tv_usec + tv.tv_sec * 1000000;
	long end_time = current_time + uwait;

	while(current_time <= end_time && nRead < (ssize_t)toRead)
	{
		ret = readWithTimeout(buf + nRead, toRead - nRead, end_time - current_time);
		if(ret < 0)
			return -1;
		nRead += ret;
		if(gettimeofday(&tv, NULL) != 0)
			return -1;
		current_time = tv.tv_usec + tv.tv_sec * 1000000;
	}
	return nRead;
}

int DVLSerialPort::flushBuffers()
{
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

	return tcflush(fd, TCIOFLUSH);
}

int DVLSerialPort::flushInput()
{
    if (fd == -1) {
        return -1;
    }
        return tcflush(fd, TCIFLUSH);
}

int DVLSerialPort::readSer(unsigned char* buf, size_t size) {
    //check to see if serial port isn't working out
    if (fd == -1) {
        return -1;
    }

    u_int8_t tmpBuf;
    int ret = 0;
    size_t i;
   
    unsigned char * ucharbuf;
    
    for (i = 0; i < size;) {
 //       usleep(10000);
        ret = read(fd, &tmpBuf, 1);
        if (ret > 0) {
            *((u_int8_t*)(buf)) = tmpBuf;
            ucharbuf = (unsigned char*)buf;  // this is a hack (mine, for gcc4) and I don't like it -Tommy
	    				     // why were we using void*s in the first place anyway?
	    ucharbuf++;
	    buf = ucharbuf;
            i++;
        }

        
    }
    return i;
}
