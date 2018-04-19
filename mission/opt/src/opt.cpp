#include "opt.h"

#include <iostream>

s::vector<ExecutionPlan> generate(const ExecutionPlan& currentPlan, const s::vector<OptimizableTask>& optimizableTasks, const s::vector<TopologicalRestriction>& topologicalRestrictions) {
  s::set<int> currentTaskNames;

  for (TaskPlan task : currentPlan) currentTaskNames.insert(task.id);

  s::set<int> impermissible;
  
  for (TopologicalRestriction restriction : topologicalRestrictions) {
    if (currentTaskNames.count(restriction.beforeTask) == 0) {
      impermissible.insert(restriction.afterTask);
    }
  }

  s::vector<ExecutionPlan> possible;

  for (OptimizableTask remaining : optimizableTasks) {
    s::vector<OptimizableTask> others;
    for (OptimizableTask other : optimizableTasks) {
      if (other.id != remaining.id) others.push_back(other);
    }

    // Option A: Skip the task.
    s::vector<ExecutionPlan> skip = generate(currentPlan, others, topologicalRestrictions);
    possible.insert(possible.end(), skip.begin(), skip.end());

    // Check if topological restrictions prevent us from attempting this task in this ordering.
    if (impermissible.count(remaining.id) == 1) continue;

    // Option B: Each possible task with each possible mode.
    for (Mode mode : remaining.modes) {
    
      TaskPlan plan { remaining.id, mode };

      ExecutionPlan current;
      current.insert(current.end(), currentPlan.begin(), currentPlan.end());
      current.push_back(plan);

      // Construct an execution plan in which we execute this task with this mode.
      s::vector<ExecutionPlan> remainder = generate(current, others, topologicalRestrictions);

      possible.insert(possible.end(), remainder.begin(), remainder.end());
  
    }
  } 

  if (optimizableTasks.size() == 0)
    possible.push_back(currentPlan);

  return possible;
}

ExecutionPlanStat statCalc(s::map<int, s::map<int, double>>& distances, const ExecutionPlan& plan, const opt_capabilities& capabilities) {
  double points = 0;
  double time   = 0;
  double distance;
  int num_poss = plan.size();

  for (int i = 0; i < num_poss; i++) {
    TaskPlan current = plan[i];
    
    points += current.mode.expectedPoints;
    time   += current.mode.expectedTime;

    if (i < num_poss - 1) {
      TaskPlan next = plan[i + 1]; 
      distance = distances[current.id][next.id];
      time += distance / capabilities.speed;
    }
  }

  return ExecutionPlanStat { points, time };
}

template<typename T>
ExecutionPlan select(Criterion<T> criterion, s::map<int, s::map<int, double>> distances, s::vector<OptimizableTask> optimizableTasks, s::vector<TopologicalRestriction> topologicalRestrictions) {

  ExecutionPlan empty;
  ExecutionPlan selected;

  s::vector<ExecutionPlan> possibilities = generate(empty, optimizableTasks, topologicalRestrictions);
  
  opt_capabilities shm_opt_capabilities;
  shm_getg(opt_capabilities, shm_opt_capabilities);

  double max = -1;

  for (ExecutionPlan plan : possibilities) {
    ExecutionPlanStat stat = statCalc(distances, plan, shm_opt_capabilities);
    if (!criterion.validate(plan, stat)) continue;
    double score = criterion.score(stat);
    if (score > max) {
      selected = plan;
      max = score;
    }
  }

  return selected;

}
