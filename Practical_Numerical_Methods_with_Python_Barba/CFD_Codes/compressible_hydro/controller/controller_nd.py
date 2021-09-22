"""
Author: Rohan
Date: 01/09/16

This file contains a base controller class used to simulate compressible fluid systems.
"""


class ControllerND(object):
    def _set_boundary_conditions(self):
        raise NotImplementedError("Calling from base class!")

    def _calculate_fluxes(self, dt, ts):
        raise NotImplementedError("Calling from base class!")

    def _calculate_time_step(self):
        raise NotImplementedError("Calling from base class!")

    def _update_states(self, dt):
        raise NotImplementedError("Calling from base class!")

    def _evolve_time_step(self, ts):
        """
        Function carrying out a single timestep
        """
        assert isinstance(ts, int)

        dt = self._calculate_time_step()
        self._set_boundary_conditions()
        self._calculate_fluxes(dt, ts)
        self._update_states(dt)

        return dt

