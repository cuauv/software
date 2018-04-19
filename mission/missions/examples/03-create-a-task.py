from mission.framework.task import Task

class MyFirstTask(Task):
    def on_first_run(self, *args, **kwargs):
        self.run_count = 0
        print("MyFirstTask ran for the first time")

    def on_run(self, max_run_count, *args, **kwargs):
        print("MyFirstTask has run at time {}".format(self.this_run_time))
        self.run_count += 1

        if self.run_count >= max_run_count:
            self.finish()

    def on_finish(self, *args, **kwargs):
        print("MyFirstTask has finished after {} runs.".format(self.run_count))


run_10_times = MyFirstTask(10)
