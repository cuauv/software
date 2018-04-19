#pragma once

#define _USE_MATH_DEFINES

#include <thread>
#include <tuple>
#include <vector>
#include <numeric>
#include <cassert>
#include <cmath>
#include <map>
#include <set>

#include <libshm/c/shm.h>

namespace s = std;

typedef struct ExecutionPlanStat {
  ExecutionPlanStat(double expectedPoints, double expectedTime) : expectedPoints(expectedPoints), expectedTime(expectedTime) {}

  double expectedPoints;
  double expectedTime;
} ExecutionPlanStat;

typedef struct Mode {
  Mode(int id, double expectedPoints, double expectedTime) : id(id), expectedPoints(expectedPoints), expectedTime(expectedTime) {}

  int id;
  double expectedPoints;
  double expectedTime;

  int _id() { return id; }
  double _expectedPoints() { return expectedPoints; }
  double _expectedTime() { return expectedTime; }
  
} Mode; 

bool operator ==(const Mode& x, const Mode& y) { return x.id == y.id && x.expectedPoints == y.expectedPoints && x.expectedTime == y.expectedTime; };

typedef struct TaskPlan {
  TaskPlan(int id, Mode mode) : id(id), mode(mode) {}

  int id;
  Mode mode;

  int _id() { return id; }
  Mode _mode() { return mode; }

} TaskPlan;

bool operator ==(const TaskPlan& x, const TaskPlan& y) { return x.id == y.id && x.mode == y.mode; };

typedef s::vector<TaskPlan> ExecutionPlan;

template<typename T>
struct Criterion {
  Criterion(T validate, double (*score)(const ExecutionPlanStat&)) : validate(validate), score(score) {}

  T validate;
  double (*score)(const ExecutionPlanStat&);
};

typedef struct OptimizableTask {
  OptimizableTask(int id, s::vector<Mode> modes) : id(id), modes(modes) {} 

  int id;
  s::vector<Mode> modes;
} OptimizableTask;

bool operator ==(const OptimizableTask& x, const OptimizableTask& y) { return x.id == y.id && x.modes == y.modes; };

typedef struct TopologicalRestriction {
  TopologicalRestriction(int beforeTask, int afterTask) : beforeTask(beforeTask), afterTask(afterTask) {}

  int beforeTask;
  int afterTask;
} TopologicalRestriction;

bool operator ==(const TopologicalRestriction& x, const TopologicalRestriction& y) { return x.beforeTask == y.beforeTask && x.afterTask == y.afterTask; };

typedef s::vector<Mode> ModeList;
typedef s::vector<OptimizableTask> OptimizableTaskList;
typedef s::vector<TopologicalRestriction> TopologicalRestrictionList;
typedef s::map<int, double> DistanceMap; 
typedef s::map<int, s::map<int, double>> FullDistanceMap;

s::vector<ExecutionPlan> generate(const ExecutionPlan& currentPlan, const s::vector<OptimizableTask>& optimizableTasks, const s::vector<TopologicalRestriction>& topologicalRestrictions);

ExecutionPlanStat statCalc(s::map<int, s::map<int, double>>& distances, const ExecutionPlan& plan, const opt_capabilities& capabilities);

template<typename T>
ExecutionPlan select(Criterion<T> criteron, s::map<int, s::map<int, double>> distances, s::vector<OptimizableTask> optimizableTasks, s::vector<TopologicalRestriction> topologicalRestrictions);
