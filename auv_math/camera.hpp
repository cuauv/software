#pragma once

#include <utility>

namespace cuauv {
namespace conf {
	struct camera;
}
namespace math {

#define REFRAC_INDEX_AIR 1.0

// Source: https://en.wikipedia.org/wiki/List_of_refractive_indices
#define REFRAC_INDEX_WATER 1.330

// This is the kind of glass in the camera lenses. Source: https://en.wikipedia.org/wiki/Borosilicate_glass
#define REFRAC_INDEX_GLASS 1.517

/*
  Use Snell's law to calculate the angle in an outside medium (e.g. water) given one observed in an inside medium (e.g. air) and known material refractive indexes.
*/
double calc_angle_across_medium(double angle_in_inside_medium, double refrac_index_outside_medium, double refrac_index_inside_medium);

/* Given a sensor dimension such as height or width,
   and the focal length of the lens in use,
   computes the field of view of the camera along that dimension.
   Angle returned is in radians.  */
double calc_field_of_view(double dimension, double focal_length);

double calc_field_of_view(const conf::camera& camera, double dimension);

/* Calculates the angle in radians from a given pixel_delta from the center
   of the camera. */
double calc_angle_from_pixel_delta(int pixel_delta, double focal_length, double pixel_size);

double calc_angle_from_pixel(const conf::camera &camera, int pixel, int camera_dim);

/* Returns heading and pitch from the center of the camera to the direction
   pointed towards by the pixel coordinates (x, y). Angles in radians. */
std::pair<double, double> calc_forward_angles(const conf::camera& camera, int x, int y);

std::pair<double, double> calc_downward_angles(const conf::camera& camera, int x, int y);

double calc_angle_subtended(const conf::camera& camera, double radius, int x);

} // namespace cuauv
} // namespace math
