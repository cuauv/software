# encoding: utf-8

import math

def inverse_clamp(value, liveband):
  """
      Ensures value is in (-∞, -liveband] ∪ [liveband, ∞)
      liveband must be a positive value
  """
  if value > 0:
    value = max(value, liveband)

  if value < 0:
    value = min(value, -liveband)

  return value

def rotate(vec, degrees):
  """
      Rotates a 2D vector x, y counter-clockwise by degrees degrees
  """
  x, y = vec
  sin = math.sin(math.radians(degrees))
  cos = math.cos(math.radians(degrees))
  rot_x =  cos * x + -sin * y
  rot_y =  sin * x +  cos * y
  return rot_x, rot_y
