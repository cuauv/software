/*!
 * \file autodetector.h
 * \author Peter Tseng
 */

#ifndef _USD_AUTODETECTOR_H_
#define _USD_AUTODETECTOR_H_

#include <map>
#include <stdint.h>

class Device;
namespace sensorserial {
    class SerialPort;
}

/*!
 * \class Autodetector
 * \brief Detects devices on autodetect serial ports.
 */
class Autodetector {
public:
    /*!
     * \brief Detects a device on a port.
     *
     * \param path Path to detect a device on.
     * \param baudRate Baud rate to use.
     * \return ID of detected device, or 0 if none found.
     */
    uint16_t detect(const char* path, int baudRate);

    /*!
     * \brief Constructs an autodetector.
     *
     * \param numports Length of array of ports to probe.
     * \param ports Array of ports to probe.
     * \param devices Devices to assign ports to.
     */
    Autodetector(int numports, const char** ports, std::map<int, Device*> &devices);
};

#endif
