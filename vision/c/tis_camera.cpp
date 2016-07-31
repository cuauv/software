#include "camera_message_framework.hpp"
#include <gst/gst.h>
#include <glib.h>
#include "tiscamera/CameraList.h"
#include <stdio.h>
#include <stblib.h>

CCameraList cam_list;
CCamera* cam;
acq_loop;

static gboolean got_image_callback(GstElement *image_sink, GstBuffer *buffer,
                                   GstPad *pad, void* _data)
{
    gint width, height, bpp;
    const GstStructure *str;
    GstCaps* pad_caps;

    pad_caps = gst_pad_get_negotiated_caps( pad );

    if( pad_caps == NULL )
    {
        g_print("Error: pad has no peer or is not negotiated yet" );
        return TRUE;
    }

    str = gst_caps_get_structure (pad_caps, 0);
    gst_structure_get_int (str, "bpp", &bpp);

    str = gst_caps_get_structure (pad_caps, 0);
    //g_print("caps: %s\n", gst_caps_to_string(pad_caps)); 

    if (!gst_structure_get_int (str, "width", &width) ) {
        std::cout << "Error: Cannot get width from pipeline source" << std::endl;      
        return TRUE;
    }

    if (!gst_structure_get_int (str, "height", &width) ) {
        std::cout << "Error: Cannot get height from pipeline source" << std::endl;      
        return TRUE;
    }

    if (!gst_structure_get_int (str, "bpp", &width) ) {
        std::cout << "Error: Cannot get bpp from pipeline source" << std::endl;      
        return TRUE;
    }
    
    gst_caps_unref(pad_caps);

    /*
    switch( bpp )
    {
        case 8:
          Format = QImage::Format_Indexed8; // Not correct, since we must create the index!
          break;

        case 16:
          Format = QImage::Format_RGB555;
          break;

        case 24:
          Format = QImage::Format_RGB888;
          break;

        case 32:
          Format = QImage::Format_RGB32;
          break;
          
        default:
          g_print ("Unknown pixel format.\n");
          return TRUE;
    }
      

      if( bpp == 8 )
      {
        sprintf(Text,"Mono 8 not working.");
      }
      else
      {
    appdata->cbCount++;
    QImage *img = new QImage( (unsigned char *)GST_BUFFER_DATA(buffer),width , height, Format);
    sprintf(Text,"img_%d.jpg",appdata->cbCount);
    img->save(Text);
    delete img;
      }
    */

    return TRUE;
}

int main(int argc, char *argv[]) {
    int num_cams = cam_list.enumerate_cameras();
    if( num_cams == 0 ) {
        printf("No TIS cameras detected");
        return 0;
    }

    char* cam_name = cam_list.getUniqueCameraName(0);
    std::cout << "Found TIS camera " << cam_name << std::endl;

    cam = cam_list.SelectCamera(cam_name);
    
    acq_loop = cam->GetPipelineLoop();
    cam->SetCallback(got_image_callback, NULL); 
    cam->start();    
}
