#include <Python.h>
#include <functional>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "opt.cpp"
#include <auvlog/client.h>

#define AUVLOG(m) auvlog_log_stdout("opt", m);

namespace {

  ExecutionPlan optimize (s::map<int, s::map<int, double>> distances, double remainingTime, s::vector<OptimizableTask> optimizableTasks, s::vector<TopologicalRestriction> topologicalRestrictions) {
    Criterion <s::function<bool(const ExecutionPlan&, const ExecutionPlanStat&)>> criterion {
      [=](const ExecutionPlan& plan, const ExecutionPlanStat& stat) -> bool { return stat.expectedTime < remainingTime; },
      [](const ExecutionPlanStat& stat) -> double { return stat.expectedPoints + (1 / (1 + stat.expectedTime)); }
    };
    return select(criterion, distances, optimizableTasks, topologicalRestrictions);
  }

}

using namespace boost::python;


BOOST_PYTHON_MODULE(opt) 
{

  def("optimize", optimize);

  class_<ExecutionPlan>("ExecutionPlan")
    .def(vector_indexing_suite<ExecutionPlan>());

  class_<ModeList>("ModeList")
    .def(vector_indexing_suite<ModeList>());

  class_<OptimizableTaskList>("OptimizableTaskList")
    .def(vector_indexing_suite<OptimizableTaskList>());

  class_<TopologicalRestrictionList>("TopologicalRestrictionList")
    .def(vector_indexing_suite<TopologicalRestrictionList>());

  class_<DistanceMap>("DistanceMap")
    .def(map_indexing_suite<DistanceMap>());

  class_<FullDistanceMap>("FullDistanceMap")
    .def(map_indexing_suite<FullDistanceMap>());
  
  class_<ExecutionPlanStat>("ExecutionPlanStat", init<double, double>());
  class_<Mode>("Mode", init<int, double, double>())
    .def("id", &Mode::_id)
    .def("expectedPoints", &Mode::_expectedPoints)
    .def("expectedTime", &Mode::_expectedTime);
  class_<TaskPlan>("TaskPlan", init<int, Mode>())
    .def("id", &TaskPlan::_id)
    .def("mode", &TaskPlan::_mode);
  //class_<Criterion>("Criterion", init<s::function<bool>(ExecutionPlan), s::function<double>(ExecutionPlanStat)>());
  class_<OptimizableTask>("OptimizableTask", init<int, s::vector<Mode>>());
  class_<TopologicalRestriction>("TopologicalRestriction", init<int, int>());

}
