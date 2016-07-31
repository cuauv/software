#ifndef POD_PKT_H
#define POD_PKT_H

//#include <inttypes.h>

struct pod_pkt {
	//Pkt Header
	uint8_t dev_type;
	uint8_t dev_id;	
	uint8_t mesg_type;
	uint8_t length;

	//data
	int16_t volt; //Units of 4.88 mV
	int16_t curr; //Units of 0.78125 mA
    	int16_t temp; //Units of 0.125 deg C
	int16_t acr;  //Units of 3.125 mAh
	
	uint8_t status;
	
	uint8_t boardID;

	uint8_t checksum;

} __attribute__ ((__packed__));
	

#endif

