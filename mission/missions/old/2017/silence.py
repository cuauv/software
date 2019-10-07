import shm

from math import degrees
from mission.missions.hydrophones import ThrusterSilencer
from mission.framework.task import Task
        
class Silence(Task):
    def on_first_run(self):
        self.silencer = ThrusterSilencer()

        self.hydro_watcher = shm.watchers.watcher()
        self.hydro_watcher.watch(shm.hydrophones_results_track)

    def on_run(self):
        self.silencer()
        
        # If there are no new pings, there is nothing to do
        if not self.hydro_watcher.has_changed():
            return
        
        # There is a new ping, get the data
        results = shm.hydrophones_results_track.get()

        phases = (results.diff_phase_x, results.diff_phase_y)
        head = degrees(results.tracked_ping_heading_radians)
        elev = degrees(results.tracked_ping_elev_radians)

        in_silence = self.silencer.in_silence()

        self.logi("[PING] In Silence: %b, Phases: (%0.3f %0.3f), \
                   Heading: %0.1f, Elevation: %0.1f" % \
                   (in_silence, phases[0], phases[1], head, elev))


silence = Silence()
