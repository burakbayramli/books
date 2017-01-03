#!/usr/bin/env python
from MathModel import *
import math

class MathModel1(MathModel):
    """Extremely simple planetary motion: circles."""

    def __init__(self):
        """Visible data structures: a planet at (x,y)
           moves around another object at (x_center,y_center)
           with frequency omega, nsteps_per_round time steps
           for one rotation.
        """
        self.x_center = 0.0
        self.y_center = 0.0
        self.x = self.y = 1.0
        self.omega = 2*math.pi  # one round: t=1
        self.nsteps_per_round = 800
        MathModel.__init__(self)

    def init(self):
        """Init internal data structures; x, y, x_center,
           y_center, omega, nsteps_per_round must be set
           before calling this function.
        """
        x0 = self.x;  y0 = self.y
        xc = self.x_center;  yc = self.y_center
        rx = xc - x0;  ry = yc - y0
        self.r = math.sqrt(rx*rx + ry*ry)
        self.x_prev = x0;  self.y_prev = y0
        self.theta_0 = math.atan2(y0-yc, x0-xc)
        self.t = 0
        self.dt = self.omega/self.nsteps_per_round
        # => t=1 after one round

    def advance(self):
        """Advance the solution one time step."""
        self.x_prev = self.x
        self.y_prev = self.y
        self.t = self.t + self.dt
        self.x = self.x_center + \
                 self.r*math.cos(self.omega*self.t + self.theta_0)
        self.y = self.y_center + \
                 self.r*math.sin(self.omega*self.t + self.theta_0)

    def get_previous_state(self):
        return self.x_prev, self.y_prev

    def get_current_state(self):
        return self.x, self.y
