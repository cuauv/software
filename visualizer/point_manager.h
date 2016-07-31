#pragma once

#include <functional>
#include <vector>

#include <GL/gl.h>

#include "async_manager.h"

class Renderable;

typedef std::function<bool (GLfloat *, GLfloat *, int)> fill_data_func;

class PointManager : public AsyncManager {
  public:
    PointManager(Renderable *point_renderable, int n_points_,
                 fill_data_func fill_func_);
    void maybe_update() override;

  protected:
    void update_thread() override;

  private:
    Renderable *renderable;
    int n_points;
    fill_data_func fill_func;

    std::vector<GLfloat> ptr_pos[2];
    std::vector<GLfloat> ptr_color[2];
};
