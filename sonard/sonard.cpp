#define SONAR_MIN_RANGE 1
#define SONAR_MAX_RANGE 40

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <bvt_sdk.h>
#include <iostream>
#include <cmath>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#include <opencv/cv.h>
#include <opencv/highgui.h>

#include <opencv2/opencv.hpp>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <libshm/c/shm.h>

#include "../vision/c/camera_message_framework.hpp"

const char *sonar_ip = "169.254.178.66";
 
int sonard_max_range = SONAR_MAX_RANGE;
int sonard_min_range = SONAR_MIN_RANGE;
int shm_max_range = SONAR_MAX_RANGE;
int shm_min_range = SONAR_MIN_RANGE;

bool running = true;

void handle_sig(int sig) {
  std::cout << "Captured a a signal ("
            << strsignal(sig)
            << "). Will terminate after current action is finished." << std::endl;
  running = false;
}

void print_usage() {
  std::cout << "Usage: auv-sonard { file | net } [ filename / IP ]" << std::endl;
}

void handle_error(int ret, std::string func_name) {
    if (ret != 0) {
      const char* err_name = BVTError_GetName((RetVal) ret);
      const char* err_desc = BVTError_GetString((RetVal) ret);
      
      std::cout << func_name << " failed: " << err_name << ": " << err_desc << " (Exit code: " << ret << ")" << std::endl;

      exit(1);
    }
}

void sonar_connect(BVTSonar son, const char* sonar_ip) {
    int ret = 0;

    for (int i = 0; i < 5; i++) {
        if (running == false) { exit(1); }

        ret = BVTSonar_Open(son, "NET", sonar_ip);

        if (ret == 0) { 
            return;
        } else if (ret == BVT_CONNECTION_TIMEDOUT) {
            std::cout << "Connection timed out, trying again" << std::endl;
            continue;
        }
        handle_error(ret, "BVTSonar_Open");
    }
    
    std::cout << "Giving up" << std::endl;
    handle_error(ret, "BVTSonar_Open");
} 

int main(int argc, char* argv[]) {
    if (argc > 3) {
        print_usage();
        return 1;
    }

    signal(SIGINT, handle_sig);
    signal(SIGTERM, handle_sig);

    BVTSonar son = BVTSonar_Create();
    if (son == NULL) {
      std::cout << "BVTSonar_Create failed." << std::endl;
      return 1;
    }
    
    bool using_file;
    
    if (argc == 1) {
        using_file = false;
    } else {
        using_file = (strcmp(argv[1], "FILE") == 0) || (strcmp(argv[1], "file") == 0);
    }
        

    int ret = 0;

    // Try to connect to the sonar, or open sonar file
    if (using_file) {
        if (argc != 3) {
            print_usage();
            return 1;
        } else {
          ret = BVTSonar_Open(son, "FILE", argv[2]);
          handle_error(ret, "BVTSonar_Open");
        }
    } else {
        if (argc == 1) {
          std::cout << "No sonar source specified, defaulting to networked sonar on " << sonar_ip << std::endl;
          sonar_connect(son, sonar_ip);
        } else if (argc == 3 && (strcmp(argv[1], "NET") == 0 || strcmp(argv[1], "net") == 0)) {
          sonar_connect(son, argv[2]);
        } else {
          print_usage();
          return 1;
        }
    }

    int heads = BVTSonar_GetHeadCount(son);
    std::cout << "Sonar heads detected: " << heads << "." << std::endl;

    BVTHead head = NULL;
    ret = BVTSonar_GetHead(son, using_file ? 0 : 1, &head);
    handle_error(ret, "BVTSonar_GetHead");

    std::cout << "Detected sonar minimum range: " << BVTHead_GetMinimumRange(head) << std::endl;
    std::cout << "Detected sonar maximum range: " << BVTHead_GetMaximumRange(head) << std::endl;
    std::cout << "Detected sonar image processing method: " << BVTHead_GetImageProcessingMethod(head) << std::endl;

    shm_init();
    shm_get(sonar, max_range, shm_max_range);
    shm_get(sonar, min_range, shm_min_range);
    BVTHead_SetRange(head, shm_min_range, shm_max_range);

  
    BVTHead_SetFluidType(head, 1);
    int res = BVTHead_GetFluidType(head);
    std::cout << "Fluid type set to: " << res << std::endl;

    BVTHead_SetImageFilterFlags(head, 16); // mean 9x2 in theory

    res = BVTHead_GetImageFilterFlags(head);
    std::cout << "Filter flags set to: " << res << std::endl;

    BVTHead_SetGainAdjustment(head, 0);

    res = BVTHead_GetGainAdjustment(head);
    std::cout << "Gain adjustment is: " << res << std::endl;

    res = BVTHead_GetTVGSlope(head);
    std::cout << "Time-variable gain slope is: " << res << std::endl;

    res = BVTHead_GetImageProcessingMethod(head);
    std::cout << "Image processing method is: " << res << std::endl;

    res = BVTHead_GetStartRange(head);
    std::cout << "Start range is: " << res << std::endl;

    res = BVTHead_GetStopRange(head);
    std::cout << "Stop range is: " << res << std::endl;

    res = BVTHead_GetSoundSpeed(head);
    std::cout << "Sound speed is: " << res << std::endl;

    BVTHead_SetRangeDataThreshold(head, 30000);

    BVTHead_SetImageRes(head, 3);

    BVTHead_SetTxEnable(head, 0);


    BVTHead_SetMountType(head, 3); // who knows what this does, probably nothing

    res = BVTHead_GetMountType(head);
    std::cout << "Mount type is: " << res << std::endl;

    BVTHead_SetImageType(head, 0);

    std::cout << "Setting range to [" << shm_min_range << ", " << shm_max_range << "]." << std::endl;

    // Detect width and height of sonar image
    BVTPing ping = NULL;
    ret = BVTHead_GetPing(head, 0, &ping);
    handle_error(ret, "BVTSonar_GetPing");

    BVTMagImage img;
    ret = BVTPing_GetImage(ping, &img);
    handle_error(ret, "BVTSonar_GetImage");

    int height = BVTMagImage_GetHeight(img);
    int width = BVTMagImage_GetWidth(img);

    BVTPing_Destroy(ping);
    BVTMagImage_Destroy(img);

    message_framework_p msg_framework = create_message_framework("sonar", width * height * 3);
    if (msg_framework == NULL) { return 0; }

    int i = 0;
    cv::Mat mat;

    long image_acq_time_in_ms;
    long prev_write_time = 0;
    struct timeval tv;

    int num_pings = BVTHead_GetPingCount(head);
    sonar shm_sonar;
 
    while (running) {
        if (i == num_pings) {
            i = 0;
        }
        
        /*
        if (!using_file) {
            shm_get(sonar, max_range, shm_max_range);
            shm_get(sonar, min_range, shm_min_range);           
            if (shm_max_range != sonard_max_range || shm_min_range != sonard_min_range) {
                sonard_max_range = shm_max_range;
                sonard_min_range = shm_min_range;
                BVTHead_SetRange(head, sonard_min_range, sonard_max_range); 

                std::cout << "New sonar minimum range: " << BVTHead_GetMinimumRange(head) << std::endl;
                std::cout << "New sonar maximum range: " << BVTHead_GetMaximumRange(head) << std::endl;

            }
        }
        */

        BVTPing ping = NULL;
        ret = BVTHead_GetPing(head, i, &ping);
        handle_error(ret, "BVTHead_GetPing");


        BVTMagImage img;
        ret = BVTPing_GetImage(ping, &img);
        handle_error(ret, "BVTPing_GetImage");

        height = BVTMagImage_GetHeight(img);
        width = BVTMagImage_GetWidth(img);

        shm_getg(sonar, shm_sonar);
        shm_sonar.range_resolution = BVTMagImage_GetRangeResolution(img);
        shm_sonar.bearing_resolution = BVTMagImage_GetBearingResolution(img);
        shm_sonar.origin_row = BVTMagImage_GetOriginRow(img);
        shm_sonar.origin_col = BVTMagImage_GetOriginCol(img);
        shm_sonar.fov_min_angle = BVTMagImage_GetFOVMinAngle(img);
        shm_sonar.fov_max_angle = BVTMagImage_GetFOVMaxAngle(img);
        shm_setg(sonar, shm_sonar);

        cv::Mat new_mat;
        mat = cv::Mat(cvSize(width, height), CV_16U, BVTMagImage_GetBits(img), width * 2);
        mat.convertTo(new_mat, CV_8UC1);

        gettimeofday(&tv, NULL);
        image_acq_time_in_ms = (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000;

        // Enforce update rate of 15 Hz if reading from file (same as real sonar)
        if (using_file) {
            while ((image_acq_time_in_ms - prev_write_time) < 66) {
                usleep(1000);

                gettimeofday(&tv, NULL);
                image_acq_time_in_ms = (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000;
            }
        }

        cv::Mat color;
        cv::cvtColor(new_mat, color, CV_GRAY2BGR);

        // Send the frame to the vision system
        write_frame(msg_framework, color.data, image_acq_time_in_ms, width, height, 3);
        prev_write_time = image_acq_time_in_ms;

        BVTMagImage_Destroy(img);
        BVTPing_Destroy(ping);

        i++;

    }
    std::cout << "Disconnecting from vision system" << std::endl;
    cleanup_message_framework(msg_framework);
    std::cout << "Disconnecting from sonar" << std::endl;
    BVTSonar_Destroy(son);
    return 0;
}
