#include <cmath>
#include <utility>
#include <conf/vehicle.hpp>
#include "camera.hpp"

namespace cuauv {
namespace math {

double calc_angle_across_medium(double angle_in_inside_medium, double refrac_index_outside_medium, double refrac_index_inside_medium) {
  /*  
    t_1 theta in outside, t_2 theta in inside ==> we have t_2, want t_1
    sin t_1 / sin t_2 = n2 / n1
    sin t_1 = n2 / n1 sin t_2
    t_1 = asin(n2 / n1 sin t_2)
  */
  
  return asin( (refrac_index_inside_medium / refrac_index_outside_medium) * sin (angle_in_inside_medium) );
}

double calc_field_of_view(const conf::camera& camera, double dimension) {
  return calc_field_of_view(dimension, camera.focal_length);
}

double calc_field_of_view(double dimension, double focal_length) {
	return 2 * atan((dimension / 2) / focal_length);
}

double calc_angle_from_pixel_delta(int pixel_delta, double focal_length,
                                   double pixel_size) {
	// It is "as if" the sensor was smaller.
  // See https://en.wikipedia.org/wiki/Angle_of_view for a nice picture.
  double angle_in_air   = calc_field_of_view(2 * pixel_delta * pixel_size, focal_length) / 2;
  // double angle_in_water = calc_angle_across_medium(angle_in_air, REFRAC_INDEX_WATER, REFRAC_INDEX_AIR);
  return angle_in_air;
}

double calc_angle_from_pixel(const conf::camera &camera, int pixel, int camera_dim) {
  int pixel_delta = pixel - camera_dim / 2;
	double pixel_size = camera.sensor_width / camera.width;
  return calc_angle_from_pixel_delta(pixel_delta, camera.focal_length, pixel_size);
}

std::pair<double, double> calc_forward_angles(const conf::camera& camera, int x, int y) {
  double heading = calc_angle_from_pixel(camera, x, camera.width);
  double pitch = -calc_angle_from_pixel(camera, y, camera.height);
	return std::make_pair(heading, pitch);
}

std::pair<double, double> calc_downward_angles(const conf::camera& camera, int x, int y) {
	auto forward_angles = calc_forward_angles(camera, x, y);
	int center_x = camera.width / 2, center_y = camera.height / 2;

	double heading;
	if (center_y == y) {
		heading = (x >= center_x) ? 0 : M_PI;
	} else {
		heading = atan((x - center_x) / (center_y - y));
	}
	if (x < center_x) heading += M_PI;
	double pitch = forward_angles.second - M_PI / 2;
	return std::make_pair(heading, pitch);
}

double calc_angle_subtended(const conf::camera& camera, double radius, int x) {
  return std::fabs(calc_angle_from_pixel(camera, x + radius, camera.width) -
                   calc_angle_from_pixel(camera, x, camera.width));
}

} // namespace cuauv
} // namespace math
