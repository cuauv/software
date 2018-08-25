import math
from control.pid import DynamicPID
from mission.framework.helpers import call_if_function, within_deadband
from mission.framework.position import PositionalControl
from mission.framework.movement import Heading, VelocityY, VelocityX, RelativeToCurrentDepth, RelativeToCurrentHeading, clamp_target_to_range
from mission.framework.task import Task


# TODO: Documentation!
class PIDLoop(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid = DynamicPID()

    def on_run(self, input_value, output_function, target=0, modulo_error=False, deadband=1, p=1, d=0, i=0,
               negate=False, max_out=None, min_target=None, max_target=None, *args, **kwargs):
        # TODO: minimum_output too?
        input_value = call_if_function(input_value)
        target = call_if_function(target)

        if max_out is None:
            max_out = float('inf')

        output = self.pid.tick(value=input_value, desired=target, p=p, d=d, i=i)
        output = math.copysign(min(abs(output), max_out), output)
        output = -output if negate else output
        output = clamp_target_to_range(target=output, min_target=min_target, max_target=max_target)

        output_function(output)

        if within_deadband(input_value, target, deadband=deadband, use_mod_error=modulo_error):
            # TODO: Should this zero on finish? Or set to I term?
            self.finish()

class CameraTarget(Task):
    def on_run(self, point, target, deadband=(0.01875, 0.01875), px=None, ix=0, dx=0, py=None, iy=0, dy=0,
               max_out=None, valid=True, min_target_x=None, max_target_x=None,
               min_target_y=None, max_target_y=None, *args, **kwargs):
        if px is None:
            px = self.px_default
        if py is None:
            py = self.py_default

        try:
            max_out_x, max_out_y = call_if_function(max_out)
        except:
            max_out_x = max_out_y = call_if_function(max_out)

        if valid:
            point = call_if_function(point)
            target = call_if_function(target)
            self.pid_loop_x(input_value=point[0], p=px, i=ix, d=dx, target=target[0], deadband=deadband[0], max_out=max_out_x, min_target=min_target_x, max_target=max_target_x)
            self.pid_loop_y(input_value=point[1], p=py, i=iy, d=dy, target=target[1], deadband=deadband[1], max_out=max_out_x, min_target=min_target_y, max_target=max_target_y)
        else:
           self.stop()

        if self.pid_loop_x.finished and self.pid_loop_y.finished:
            # TODO: Should the output be zeroed on finish?
            self.finish()

class ForwardTarget(CameraTarget):
    def on_first_run(self, depth_bounds=(None, None), *args, **kwargs):
        self.pid_loop_x = PIDLoop(output_function=VelocityY(), negate=True)
        self.pid_loop_y = PIDLoop(output_function=RelativeToCurrentDepth(min_target=depth_bounds[0], max_target=depth_bounds[1]), negate=True)
        self.px_default = 0.8
        self.py_default = 0.8
        PositionalControl(False)()

    def stop(self):
        VelocityY(0)()
        RelativeToCurrentDepth(0)()

class DownwardTarget(CameraTarget):
    def on_first_run(self, *args, **kwargs):
        # x-axis on the camera corresponds to sway axis for the sub
        self.pid_loop_x = PIDLoop(output_function=VelocityY(), negate=True)
        self.pid_loop_y = PIDLoop(output_function=VelocityX(), negate=False)
        self.px_default = 0.4
        self.py_default = 0.8
        PositionalControl(False)()

    def stop(self):
        VelocityY(0)()
        VelocityX(0)()

class HeadingTarget(CameraTarget):
    def on_first_run(self, depth_bounds=(None, None), *args, **kwargs):
        self.pid_loop_x = PIDLoop(output_function=RelativeToCurrentHeading(), negate=True)
        self.pid_loop_y = PIDLoop(output_function=RelativeToCurrentDepth(min_target=depth_bounds[0], max_target=depth_bounds[1]), negate=True)
        self.px_default = 8
        self.py_default = 0.8

    def stop(self):
        RelativeToCurrentDepth(0)()
        RelativeToCurrentHeading(0)()

class DownwardAlign(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop_heading = PIDLoop(output_function=RelativeToCurrentHeading(), modulo_error=True)

    def on_run(self, angle, deadband=0.05, p=.8, i=0, d=0, target=0, modulo_error=True):
        angle = call_if_function(angle)
        self.pid_loop_heading(input_value=angle, p=p, i=i, d=d, target=target, deadband=deadband, negate=True)

        if self.pid_loop_heading.finished:
            self.finish()
