# -*- coding: utf-8 -*-
"""
BSD 3-Clause License
Copyright (c) 2023, Donald N. Bockoven III
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

import math


class R2Node:
    def __init__(self, x, y):
        """

        Parameters
        ----------
        x : float
            x-axis coordinate of the node in R2.
        y : float
            y-axis coordinate of the node in R2.
        uid : int
            unique node number.

        Returns
        -------
        None.

        """

        self.uid = None
        self.x = x
        self.y = y

        # Restraints [Ux, Uy, Rz]
        self.restraints_key = ["Ux", "Uy", "Rz"]
        self.restraints = [1, 1, 1]

        # Spring Restraint [kux, kuy, krz]
        # Spring Stiffness should be 0 for a restrained direction
        # Spring is still a DOF for the node
        self._spring_stiffness = [0, 0, 0]

        # Directionality of spring
        # 0 = bidirectional resistance
        # 1 = spring resists positive axis displacement
        # -1 = spring resists negative axis displacement
        self._spring_direction = [0, 0, 0]

        # Spring Stiffness Multiplier
        # This will be part of Tension/Compression non-linear analysis
        # and will soften or deactivate entirely the spring
        # This needs to be done on a per combination basis so this
        # is a series of dicts for each DOF
        self._springUxmulti = {}
        self._springUymulti = {}
        self._springRzmulti = {}

        # Enforced Displacement [Ux, Uy, Rz]
        # Enforced Displacements count as a restrained DOF if that
        # DOF is not already restrained by a support condition
        self._enforced_displacements = [0, 0, 0]

        # Dict of Loads by case
        self.loads = {}

        # Dict of Displacements by combo
        self.displacements = {}

        # Dict of Reactions by combo
        self.reactions = {}

        # Flags
        self._isSpring = False
        self._isNonLinear = False

    def __str__(self):
        str = f"Node:{self.uid}\n"
        str += f"({self.x},{self.y})\n"

        if sum(self.restraints) != 0:
            str += "Restraints\n"
            str += "-" * 11 + "\n"

            for i, r in enumerate(self.restraints):
                if r != 0:
                    str += f"{self.restraints_key[i]}"
        return str

    def x_displaced(self, load_combination, scale=1.0):
        delta = self.displacements.get(load_combination.name, [0, 0, 0])

        return self.x + (delta[0] * scale)

    def y_displaced(self, load_combination, scale=1.0):
        delta = self.displacements.get(load_combination.name, [0, 0, 0])

        return self.y + (delta[1] * scale)

    def distance(self, other):
        """

        Parameters
        ----------
        other : R2Node
            another node defined by this class.

        Returns
        -------
        distance: float
            Euclidean distance in R2

        """

        dx = self.x - other.x
        dy = self.y - other.y

        return math.sqrt((dx * dx) + (dy * dy))

    def restrainUx(self):
        self.restraints[0] = 1

    def restrainUy(self):
        self.restraints[1] = 1

    def restrainMz(self):
        self.restraints[2] = 1

    def restrainAll(self):
        self.restraints = [1, 1, 1]

    def releaseUx(self):
        self.restraints[0] = 0

    def releaseUy(self):
        self.restraints[1] = 0

    def releaseMz(self):
        self.restraints[2] = 0

    def releaseAll(self):
        self.restraints = [0, 0, 0]

    def applySpringUx(self, k=100, direction=0):
        self.restraints[0] = 0
        self._spring_stiffness[0] = k
        self._spring_direction[0] = direction

        self._isSpring = True

        if direction != 0:
            self._isNonLinear = True

    def applySpringUy(self, k=100, direction=0):
        self.restraints[1] = 0
        self._spring_stiffness[1] = k
        self._spring_direction[1] = direction

        self._isSpring = True

        if direction != 0:
            self._isNonLinear = True

    def applySpringRz(self, k=100, direction=0):
        self.restraints[2] = 0
        self._spring_stiffness[2] = k
        self._spring_direction[2] = direction

        self._isSpring = True

        if direction != 0:
            self._isNonLinear = True
