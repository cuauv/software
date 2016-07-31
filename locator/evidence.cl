/*
This OpenCL file gives function for Locator to use to compute
probabilities based upon new pieces of evidence.

Computations are performed on the GPU for high performance.

The syntax of OpenCL is based on C99 so you might want to set up 
syntax highlighting for C, like so on Vim
:set syn=c
*/

/* Utility functions */

/* This is a replacement for fmod, since fmod returns a
values in the range (-2pi,2pi) not (0,2pi).
*/
float mod(float x, float p)
{
    return x - p * floor(x/p);
}

/* angle_from computes the angle to the point (x,y) to
the point (0,0) and where an angle of 0 is taken to be
going through the line of angle 'angle_offset' from the +y axis.
*/
float angle_from(float x, float y, 
                float angle_offset)
{
    float angle = atan2(y, x);
    angle = mod(angle-angle_offset,  2*M_PI);

    angle -= (2*M_PI) * (angle > M_PI); // Convert to (-pi,pi)
    return angle;
}

/* Angle Clamps
These define a variety of clampings for different angular spreads of evidence
*/
// Larger coefficient -> narrower clamping
float angle_clamp(float angle, float coefficient)
{
    return fmax( fmin( 1.05 - coefficient*fabs(angle), 1), 0);
}
// Specific functions; deprecated for above
float narrow_angle_clamp(float angle) {
    return fmax( fmin( 1.05 - 6*fabs(angle), 1), 0);
}
float inferred_angle_clamp(float angle) {
    return fmax( fmin( 1.05 - 4*fabs(angle), 1), 0);
}
float wide_angle_clamp(float angle) {
    return fmax( fmin( 1.05 - 2*fabs(angle), 1), 0);
}


/* Kernels
These functions are the top-level ones that get called from Python
*/


/* This function updates 'probabilities' array at each point.
xs and ys are arrays giving positions of each point in probabilities
x,y gives the position of the sub
angle_offset is the angle that the sub is looking in
The rest of the variables describe a conical-shaped region that
is of interest to us.
max_dist, min_dist give the extents of the cone in the direction towards or away from the sub.
width is an ununituitive measure of how wide to make the cone; see  the angle clamp functions.
in_weight gives the probability of the evidence being observed given that
the target is at a location inside of the conical region
out_weight gives the probability of the evidence being observed given that
the target is at a location outside the conical region

This function is invoked once per element inside 'probabilities'
and the global ID defines which point each instance is to work on.
*/

__kernel void evidence(__global const float *xs,
                  __global const float *ys, 
                  __global float *probabilities,
                  float x,
                  float y,
                  float angle_offset,
                  float min_dist,
                  float max_dist,
                  float width,
                  float in_weight,
                  float out_weight
                  )
{
    int gid = get_global_id(0);

    // Find relative displacement from the sub
    float rel_x = xs[gid]-x;
    float rel_y = ys[gid]-y;

    // Determine angle in range (-pi,pi) relative to the direction of interest
    float angle = angle_from( rel_x, rel_y, angle_offset);

    // Restrict to the angles of interest
    float evidence = angle_clamp(angle, width);

    // Compute distance from sub
    float dist = (rel_x)*(rel_x) + (rel_y)*(rel_y);

    // Exclude points outside of our region of interest
    evidence *= (dist > min_dist) * (dist < max_dist);

    // Actually update the probabilities
    probabilities[gid] *= (in_weight-out_weight)*evidence + out_weight;
}
