#!/usr/bin/env python

class MathModel:
    def __init__(self):
        self.t = 0.0

    def init(self):
        """Init internal data structures."""
        raise NotImplementedError

    def advance(self):
        """Advance the solution one time step."""
        raise NotImplementedError

    def get_previous_state(self):
        """Return state at the previous time level."""
        raise NotImplementedError

    def get_current_state(self):
        """Return state at the current time level."""
        raise NotImplementedError

    def time(self):
        """Return current time in the math. model."""
        return self.t
