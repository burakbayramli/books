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
import numpy as np

import pyMAOS.loading as loadtypes


class R2Truss:
    def __init__(self, inode, jnode, material, section):
        self.type = "TRUSS"

        self.inode = inode
        self.jnode = jnode

        self.material = material
        self.section = section

        self.uid = None

        # Dictionaries of End Forces from combo
        self.end_forces_local = {}
        self.end_forces_global = {}

        # Dictionary to Remove element from analysis combo
        self.isoff = {}

        # Flags
        self._TensionOnly = False
        self._CompressionOnly = False
        self._stations = False

    @property
    def length(self):
        """
        Sets the member length from the i and j nodes
        """

        self._length = self.inode.distance(self.jnode)

        return self._length

    def set_tension_only(self):
        self._TensionOnly = True
        self.clear_compression_only()

    def set_compression_only(self):
        self._CompressionOnly = True
        self.clear_tension_only()

    def clear_tension_only(self):
        self._TensionOnly = False

    def clear_compression_only(self):
        self._CompressionOnly = False

    def k(self):
        E = self.material.E
        A = self.section.Area
        L = self.length

        k11 = (A * E) / L
        k21 = 0
        k31 = 0
        k41 = -1 * (A * E) / L
        k51 = 0
        k61 = 0

        k12 = 0
        k22 = 0
        k32 = 0
        k42 = 0
        k52 = 0
        k62 = 0

        k13 = 0
        k23 = 0
        k33 = 0
        k43 = 0
        k53 = 0
        k63 = 0

        k14 = k41
        k24 = 0
        k34 = 0
        k44 = k11
        k54 = 0
        k64 = 0

        k15 = 0
        k25 = 0
        k35 = 0
        k45 = 0
        k55 = 0
        k65 = 0

        k16 = 0
        k26 = 0
        k36 = 0
        k46 = 0
        k56 = 0
        k66 = 0

        k = np.matrix(
            [
                [k11, k12, k13, k14, k15, k16],
                [k21, k22, k23, k24, k25, k26],
                [k31, k32, k33, k34, k35, k36],
                [k41, k42, k43, k44, k45, k46],
                [k51, k52, k53, k54, k55, k56],
                [k61, k62, k63, k64, k65, k66],
            ]
        )

        return k

    def T(self):
        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        T = np.matrix(
            [
                [c, s, 0, 0, 0, 0],
                [-s, c, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0],
                [0, 0, 0, c, s, 0],
                [0, 0, 0, -s, c, 0],
                [0, 0, 0, 0, 0, 1],
            ]
        )

        return T

    def kglobal(self):
        k = self.k()
        T = self.T()

        kglobal = np.matmul(np.matmul(np.transpose(T), k), T)

        return kglobal

    def Dglobal(self, load_combination):
        # Global nodal displacement vector
        D = np.zeros(6)

        iD = self.inode.displacements[load_combination.name]
        jD = self.jnode.displacements[load_combination.name]

        # Populate Displacement Vector
        D[0] = iD[0]
        D[1] = iD[1]
        D[2] = iD[2]
        D[3] = jD[0]
        D[4] = jD[1]
        D[5] = jD[2]

        return D

    def Dlocal(self, load_combination):
        Dglobal = self.Dglobal(load_combination)

        Dlocal = np.matmul(self.T(), Dglobal)

        return Dlocal

    def Flocal(self, load_combination):
        Dlocal = self.Dlocal(load_combination)

        FL = np.matmul(self.k(), Dlocal.T)

        self.end_forces_local[load_combination.name] = FL

    def Fglobal(self, load_combination):
        Dglobal = self.Dglobal(load_combination)

        # global stiffness matrix
        KG = self.kglobal()

        FG = np.matmul(KG, Dglobal)

        self.end_forces_global[load_combination.name] = FG

        self.Flocal(load_combination)

        return FG

    def stations(self, num_stations=3):
        """
        define general computation points along the beam length for shear,
        moment, slope, and deflection plots
        """

        # parametric list of stations between 0 and 1'
        eta = [0 + i * (1 / num_stations) for i in range(num_stations + 1)]

        stations = [self.length * i for i in eta]

        # Make sure the first and last stations do not exceed the beam
        if stations[0] < 0:
            stations[0] = 0

        if stations[-1] > self.length:
            stations[-1] = self.length

        # Remove duplicate locations
        self.calcstations = sorted(set(stations))

        self._stations = True

    def Alocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecewise functions to build the total function from the loading
        ax = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)

        ax = ax.combine(fxi.Ax, 1, 1)
        ax = ax.combine(fxj.Ax, 1, 1)

        axlocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            a = ax.evaluate(x)

            axlocal_span[i, 0] = x
            axlocal_span[i, 1] = a * scale

        return axlocal_span

    def Aglobal_plot(self, load_combination, scale):
        axlocal_plot = self.Alocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        axglobal_plot = np.matmul(axlocal_plot, R)

        return axglobal_plot

    def Dlocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        Dlocal = self.Dlocal(load_combination)

        # Parametric Functions defining a linear relationship for deflection
        # in each axis based on the Ux and Uy nodal displacements
        Dx = lambda x: Dlocal[0, 0] + (x / self.length) * (Dlocal[0, 3] - Dlocal[0, 0])
        Dy = lambda x: Dlocal[0, 1] + (x / self.length) * (Dlocal[0, 4] - Dlocal[0, 1])

        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecewise functions to build the total function from the loading
        dx = loadtypes.Piecewise_Polynomial()
        dy = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)

        dx = dx.combine(fxi.Dx, 1, 1)
        dx = dx.combine(fxj.Dx, 1, 1)

        dlocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            dxl = dx.evaluate(x) + Dx(0)
            dyl = dy.evaluate(x) + Dy(x)

            dlocal_span[i, 0] = x + (dxl * scale)
            dlocal_span[i, 1] = dyl * scale

        return dlocal_span

    def Dglobal_plot(self, load_combination, scale=1):
        dlocal_plot = self.Dlocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        dglobal_plot = np.matmul(dlocal_plot, R)

        return dglobal_plot


class R2Frame:
    def __init__(self, inode, jnode, material, section):
        self.type = "FRAME"

        self.inode = inode
        self.jnode = jnode

        self.material = material
        self.section = section

        self.uid = None

        self.hinges = [0, 0]

        self.loads = []

        self.end_forces_local = {}

        self.end_forces_global = {}

        self.fixed_end_forces = {}

        # Internal Functions
        # Dictionary key for each combination
        self.Wx = {}
        self.Wy = {}
        self.A = {}
        self.Vy = {}
        self.Mz = {}
        self.Sz = {}
        self.Dx = {}
        self.Dy = {}

        # Flags
        self._stations = False
        self._loaded = False

    @property
    def length(self):
        """
        Sets the member length from the i and j nodes
        """

        self._length = self.inode.distance(self.jnode)

        return self._length

    def hinge_i(self):
        self.hinges[0] = 1

    def hinge_j(self):
        self.hinges[1] = 1

    def fix_i(self):
        self.hinges[0] = 0

    def fix_j(self):
        self.hinges[1] = 0

    def add_point_load(self, p, a, case="D", direction="y", location_percent=False):
        """

        Parameters
        ----------
        p : TYPE
            DESCRIPTION.
        a : TYPE
            DESCRIPTION.
        case : TYPE
            DESCRIPTION.
        direction : TYPE
            DESCRIPTION.
        location_percent : TYPE, optional
            DESCRIPTION. The default is False.

        Returns
        -------
        None.

        """

        if location_percent:
            a = (a / 100) * self.length
        if direction == "Y" or direction == "X":
            # Load is applied in the gloabl axis

            c = (self.jnode.x - self.inode.x) / self.length
            s = (self.jnode.y - self.inode.y) / self.length

            if direction == "Y":
                pyy = c * p
                pxx = s * p
            else:
                pyy = -1 * s * p
                pxx = c * p
            self.loads.append(loadtypes.R2_Axial_Load(pxx, a, self, loadcase=case))
            self.loads.append(loadtypes.R2_Point_Load(pyy, a, self, loadcase=case))
        else:
            # Load is applied in the local member axis

            if direction == "xx":
                self.loads.append(loadtypes.R2_Axial_Load(p, a, self, loadcase=case))
            else:
                self.loads.append(loadtypes.R2_Point_Load(p, a, self, loadcase=case))

        self._stations = False
        self._loaded = True

    def add_distributed_load(
        self,
        wi,
        wj,
        a,
        b,
        case="D",
        direction="y",
        location_percent=False,
        projected=False,
    ):
        """

        Parameters
        ----------
        wi : TYPE
            DESCRIPTION.
        wj : TYPE
            DESCRIPTION.
        a : TYPE
            DESCRIPTION.
        b : TYPE
            DESCRIPTION.
        case : TYPE
            DESCRIPTION.
        direction : TYPE
            DESCRIPTION.
        location_percent : TYPE, optional
            DESCRIPTION. The default is False.

        Returns
        -------
        None.

        """

        if location_percent:
            a = (a / 100) * self.length
            b = (b / 100) * self.length
        if direction == "Y" or direction == "X":
            # Load is applied in the gloabl axis

            c = (self.jnode.x - self.inode.x) / self.length
            s = (self.jnode.y - self.inode.y) / self.length

            if direction == "Y":
                if projected:
                    wi = c * wi
                    wj = c * wj

                wyyi = c * wi
                wyyj = c * wj
                wxxi = s * wi
                wxxj = s * wj
            else:
                if projected:
                    wi = s * wi
                    wj = s * wj

                wyyi = -1 * s * wi
                wyyj = -1 * s * wj
                wxxi = c * wi
                wxxj = c * wj
            self.loads.append(
                loadtypes.R2_Axial_Linear_Load(wxxi, wxxj, a, b, self, loadcase=case)
            )
            self.loads.append(
                loadtypes.R2_Linear_Load(wyyi, wyyj, a, b, self, loadcase=case)
            )
        else:
            # Load is applied in the local member axis

            if direction == "xx":
                self.loads.append(
                    loadtypes.R2_Axial_Linear_Load(wi, wj, a, b, self, loadcase=case)
                )
            else:
                self.loads.append(
                    loadtypes.R2_Linear_Load(wi, wj, a, b, self, loadcase=case)
                )

        self._stations = False
        self._loaded = True

    def add_moment_load(self, m, a, case, location_percent=False):
        """

        Parameters
        ----------
        m : TYPE
            DESCRIPTION.
        a : TYPE
            DESCRIPTION.
        case : TYPE
            DESCRIPTION.
        location_percent : TYPE, optional
            DESCRIPTION. The default is False.

        Returns
        -------
        None.

        """

        if location_percent:
            a = (a / 100) * self.length
        self.loads.append(loadtypes.R2_Point_Moment(m, a, self, loadcase=case))

        self._stations = False
        self._loaded = True

    def FEF(self, load_combination):
        """

        Parameters
        ----------
        case : TYPE
            DESCRIPTION.

        Returns
        -------
        fef : TYPE
            DESCRIPTION.

        """

        fef = np.array([0, 0, 0, 0, 0, 0])

        for load in self.loads:
            load_factor = load_combination.factors.get(load.loadcase, 0)

            if load_factor != 0:
                loadfef = np.array([load_factor * i for i in load.FEF()])
                fef = fef + loadfef

        if self.hinges == [1, 0]:
            Mi = fef[2]
            L = self.length

            fef[1] = fef[1] - ((3 / (2 * L)) * Mi)
            fef[2] = 0
            fef[4] = fef[4] + ((3 / (2 * L)) * Mi)
            fef[5] = fef[5] - (Mi / 2)

        elif self.hinges == [0, 1]:
            Mj = fef[5]
            L = self.length

            fef[1] = fef[1] - ((3 / (2 * L)) * Mj)
            fef[2] = fef[2] - (Mj / 2)
            fef[4] = fef[4] + ((3 / (2 * L)) * Mj)
            fef[5] = 0

        elif self.hinges == [1, 1]:
            Mi = fef[2]
            Mj = fef[5]
            L = self.length

            fef[1] = fef[1] - ((Mj + Mi) / L)
            fef[2] = 0
            fef[4] = fef[4] + ((Mj + Mi) / L)
            fef[5] = 0

        else:
            pass

        return fef

    def FEFglobal(self, load_combination):
        """

        Parameters
        ----------
        case : TYPE
            DESCRIPTION.

        Returns
        -------
        TYPE
            DESCRIPTION.

        """

        fef = np.transpose(self.FEF(load_combination))
        T = self.T()

        return np.matmul(np.transpose(T), fef)

    def k(self):
        """

        Returns
        -------
        k : TYPE
            DESCRIPTION.


        """

        E = self.material.E
        Ixx = self.section.Ixx
        A = self.section.Area
        L = self.length

        if self.hinges == [1, 0]:
            k11 = (A * E) / L
            k21 = 0
            k31 = 0
            k41 = -1 * (A * E) / L
            k51 = 0
            k61 = 0

            k12 = 0
            k22 = (3 * E * Ixx) / (L * L * L)
            k32 = 0
            k42 = 0
            k52 = (-3 * E * Ixx) / (L * L * L)
            k62 = (3 * E * Ixx) / (L * L)

            k13 = 0
            k23 = 0
            k33 = 0
            k43 = 0
            k53 = 0
            k63 = 0

            k14 = -1 * (A * E) / L
            k24 = 0
            k34 = 0
            k44 = (A * E) / L
            k54 = 0
            k64 = 0

            k15 = 0
            k25 = (-3 * E * Ixx) / (L * L * L)
            k35 = 0
            k45 = 0
            k55 = (3 * E * Ixx) / (L * L * L)
            k65 = (-3 * E * Ixx) / (L * L)

            k16 = 0
            k26 = (3 * E * Ixx) / (L * L)
            k36 = 0
            k46 = 0
            k56 = (-3 * E * Ixx) / (L * L)
            k66 = (3 * E * Ixx) / L

        elif self.hinges == [0, 1]:
            k11 = (A * E) / L
            k21 = 0
            k31 = 0
            k41 = -1 * (A * E) / L
            k51 = 0
            k61 = 0

            k12 = 0
            k22 = (3 * E * Ixx) / (L * L * L)
            k32 = (3 * E * Ixx) / (L * L)
            k42 = 0
            k52 = (-3 * E * Ixx) / (L * L * L)
            k62 = 0

            k13 = 0
            k23 = (3 * E * Ixx) / (L * L)
            k33 = (3 * E * Ixx) / L
            k43 = 0
            k53 = (-3 * E * Ixx) / (L * L)
            k63 = 0

            k14 = -1 * (A * E) / L
            k24 = 0
            k34 = 0
            k44 = (A * E) / L
            k54 = 0
            k64 = 0

            k15 = 0
            k25 = (-3 * E * Ixx) / (L * L * L)
            k35 = (-3 * E * Ixx) / (L * L)
            k45 = 0
            k55 = (3 * E * Ixx) / (L * L * L)
            k65 = 0

            k16 = 0
            k26 = 0
            k36 = 0
            k46 = 0
            k56 = 0
            k66 = 0

        elif self.hinges == [1, 1]:
            k11 = (A * E) / L
            k21 = 0
            k31 = 0
            k41 = -1 * (A * E) / L
            k51 = 0
            k61 = 0

            k12 = 0
            k22 = 0
            k32 = 0
            k42 = 0
            k52 = 0
            k62 = 0

            k13 = 0
            k23 = 0
            k33 = 0
            k43 = 0
            k53 = 0
            k63 = 0

            k14 = -1 * (A * E) / L
            k24 = 0
            k34 = 0
            k44 = (A * E) / L
            k54 = 0
            k64 = 0

            k15 = 0
            k25 = 0
            k35 = 0
            k45 = 0
            k55 = 0
            k65 = 0

            k16 = 0
            k26 = 0
            k36 = 0
            k46 = 0
            k56 = 0
            k66 = 0

        else:
            k11 = (A * E) / L
            k21 = 0
            k31 = 0
            k41 = -1 * (A * E) / L
            k51 = 0
            k61 = 0

            k12 = 0
            k22 = (12 * E * Ixx) / (L * L * L)
            k32 = (6 * E * Ixx) / (L * L)
            k42 = 0
            k52 = (-12 * E * Ixx) / (L * L * L)
            k62 = (6 * E * Ixx) / (L * L)

            k13 = 0
            k23 = (6 * E * Ixx) / (L * L)
            k33 = (4 * E * Ixx) / L
            k43 = 0
            k53 = (-6 * E * Ixx) / (L * L)
            k63 = (2 * E * Ixx) / L

            k14 = -1 * (A * E) / L
            k24 = 0
            k34 = 0
            k44 = (A * E) / L
            k54 = 0
            k64 = 0

            k15 = 0
            k25 = (-12 * E * Ixx) / (L * L * L)
            k35 = (-6 * E * Ixx) / (L * L)
            k45 = 0
            k55 = (12 * E * Ixx) / (L * L * L)
            k65 = (-6 * E * Ixx) / (L * L)

            k16 = 0
            k26 = (6 * E * Ixx) / (L * L)
            k36 = (2 * E * Ixx) / L
            k46 = 0
            k56 = (-6 * E * Ixx) / (L * L)
            k66 = (4 * E * Ixx) / L

        k = np.matrix(
            [
                [k11, k12, k13, k14, k15, k16],
                [k21, k22, k23, k24, k25, k26],
                [k31, k32, k33, k34, k35, k36],
                [k41, k42, k43, k44, k45, k46],
                [k51, k52, k53, k54, k55, k56],
                [k61, k62, k63, k64, k65, k66],
            ]
        )

        return k

    def T(self):
        """

        Returns
        -------
        T : TYPE
            DESCRIPTION.

        """

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        T = np.matrix(
            [
                [c, s, 0, 0, 0, 0],
                [-s, c, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0],
                [0, 0, 0, c, s, 0],
                [0, 0, 0, -s, c, 0],
                [0, 0, 0, 0, 0, 1],
            ]
        )

        return T

    def kglobal(self):
        """

        Returns
        -------
        kglobal : TYPE
            DESCRIPTION.

        """

        k = self.k()
        T = self.T()

        kglobal = np.matmul(np.matmul(np.transpose(T), k), T)

        return kglobal

    def Dglobal(self, load_combination):
        """

        Parameters
        ----------
        combo : TYPE
            DESCRIPTION.

        Returns
        -------
        D : TYPE
            DESCRIPTION.

        """

        # Gloabl nodal displacement vector
        D = np.zeros(6)

        iD = self.inode.displacements[load_combination.name]
        jD = self.jnode.displacements[load_combination.name]

        # Populate Displacement Vector
        D[0] = iD[0]
        D[1] = iD[1]
        D[2] = iD[2]
        D[3] = jD[0]
        D[4] = jD[1]
        D[5] = jD[2]

        return D

    def Dlocal(self, load_combination):
        """

        Parameters
        ----------
        combo : TYPE
            DESCRIPTION.

        Returns
        -------
        Dlocal : TYPE
            DESCRIPTION.

        """

        Dglobal = self.Dglobal(load_combination)

        Dlocal = np.matmul(self.T(), Dglobal)

        return Dlocal

    def Flocal(self, load_combination):
        """

        Parameters
        ----------
        combo : TYPE
            DESCRIPTION.

        Returns
        -------
        None.

        """

        Dlocal = self.Dlocal(load_combination)
        Qf = np.reshape(self.FEF(load_combination), (-1, 1))

        FL = np.matmul(self.k(), Dlocal.T)

        self.end_forces_local[load_combination.name] = FL + Qf

    def Fglobal(self, load_combination):
        """

        Parameters
        ----------
        combo : TYPE
            DESCRIPTION.

        Returns
        -------
        FG : TYPE
            DESCRIPTION.

        """

        Dglobal = self.Dglobal(load_combination)
        Qfg = self.FEFglobal(load_combination)

        # global stiffness matrix
        KG = self.kglobal()

        FG = np.matmul(KG, Dglobal)

        self.end_forces_global[load_combination.name] = FG + Qfg

        self.Flocal(load_combination)

        return FG + Qfg

    def stations(self, num_stations=10):
        """
        Define evenly distributed points along the member to compute internal
        actions. Additional points are generated for load application points.

        This also generates a reduced set of points for use in the max/min
        internal action functions.

        :param num_stations: _description_, defaults to 10
        :type num_stations: int, optional
        """

        # parametric list of stations between 0 and 1'
        eta = [0 + i * (1 / num_stations) for i in range(num_stations + 1)]

        stations = [self.length * i for i in eta]
        max_stations = [0, self.length]

        if self._loaded:
            extra_stations = []

            for load in self.loads:
                if (
                    load.kind == "POINT"
                    or load.kind == "MOMENT"
                    or load.kind == "AXIAL_POINT"
                ):
                    b = min(self.length, load.a + 0.001)
                    c = max(0, load.a - 0.001)
                    extra_stations.extend([c, load.a, b])
                    max_stations.extend([c, load.a, b])

                elif load.kind == "LINE" or load.kind == "AXIAL_LINE":
                    c = min(self.length, load.b + 0.001)
                    d = max(0, load.a - 0.001)
                    extra_stations.extend([d, load.a, load.b, c])
                    max_stations.extend([d, load.a, load.b, c])
                else:
                    pass

            stations.extend(extra_stations)

        stations.sort()
        max_stations.sort()

        # Make sure the first and last stations do not exceed the beam

        if stations[0] < 0:
            stations[0] = 0

        if stations[-1] > self.length:
            stations[-1] = self.length
        
        if max_stations[0] < 0:
            max_stations[0] = 0

        if max_stations[-1] > self.length:
            max_stations[-1] = self.length

        # Remove duplicate locations
        self.calcstations = sorted(set(stations))
        self.maxstations = sorted((set(max_stations)))

        self._stations = True

    def generate_Loading_function(self, load_combination):
        wy = loadtypes.Piecewise_Polynomial()
        wx = loadtypes.Piecewise_Polynomial()

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)

                if load_factor != 0:
                    wx = wx.combine(load.Wx, 1, load_factor)
                    wy = wy.combine(load.Wy, 1, load_factor)

        self.Wx[load_combination.name] = wx
        self.Wy[load_combination.name] = wy

    def generate_Axial_function(self, load_combination):
        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecewise functions to build the total function from the loading
        ax = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fyi = loadtypes.R2_Point_Load(Fendlocal[1, 0], 0, self)
        mzi = loadtypes.R2_Point_Moment(Fendlocal[2, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)
        fyj = loadtypes.R2_Point_Load(Fendlocal[4, 0], self.length, self)
        mzj = loadtypes.R2_Point_Moment(Fendlocal[5, 0], self.length, self)

        ax = ax.combine(fxi.Ax, 1, 1)
        ax = ax.combine(fyi.Ax, 1, 1)
        ax = ax.combine(mzi.Ax, 1, 1)
        ax = ax.combine(fxj.Ax, 1, 1)
        ax = ax.combine(fyj.Ax, 1, 1)
        ax = ax.combine(mzj.Ax, 1, 1)

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)

                if load_factor != 0:
                    ax = ax.combine(load.Ax, 1, load_factor)

        self.A[load_combination.name] = ax

    def generate_Vy_function(self, load_combination):
        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecewise functions to build the total function from the loading
        vy = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fyi = loadtypes.R2_Point_Load(Fendlocal[1, 0], 0, self)
        mzi = loadtypes.R2_Point_Moment(Fendlocal[2, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)
        fyj = loadtypes.R2_Point_Load(Fendlocal[4, 0], self.length, self)
        mzj = loadtypes.R2_Point_Moment(Fendlocal[5, 0], self.length, self)

        vy = vy.combine(fxi.Vy, 1, 1)
        vy = vy.combine(fyi.Vy, 1, 1)
        vy = vy.combine(mzi.Vy, 1, 1)
        vy = vy.combine(fxj.Vy, 1, 1)
        vy = vy.combine(fyj.Vy, 1, 1)
        vy = vy.combine(mzj.Vy, 1, 1)

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)
                if load_factor != 0:
                    vy = vy.combine(load.Vy, 1, load_factor)

        self.Vy[load_combination.name] = vy

    def generate_Mz_function(self, load_combination):
        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecewise functions to build the total function from the loading
        Mzx = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fyi = loadtypes.R2_Point_Load(Fendlocal[1, 0], 0, self)
        mzi = loadtypes.R2_Point_Moment(Fendlocal[2, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)
        fyj = loadtypes.R2_Point_Load(Fendlocal[4, 0], self.length, self)
        mzj = loadtypes.R2_Point_Moment(Fendlocal[5, 0], self.length, self)

        Mzx = Mzx.combine(fxi.Mz, 1, 1)
        Mzx = Mzx.combine(fyi.Mz, 1, 1)
        Mzx = Mzx.combine(mzi.Mz, 1, 1)
        Mzx = Mzx.combine(fxj.Mz, 1, 1)
        Mzx = Mzx.combine(fyj.Mz, 1, 1)
        Mzx = Mzx.combine(mzj.Mz, 1, 1)

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)
                if load_factor != 0:
                    Mzx = Mzx.combine(load.Mz, 1, load_factor)

        self.Mz[load_combination.name] = Mzx

    def generate_Sz_function(self, load_combination):
        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecwise functions to build the total function from the loading
        Szx = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fyi = loadtypes.R2_Point_Load(Fendlocal[1, 0], 0, self)
        mzi = loadtypes.R2_Point_Moment(Fendlocal[2, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)
        fyj = loadtypes.R2_Point_Load(Fendlocal[4, 0], self.length, self)
        mzj = loadtypes.R2_Point_Moment(Fendlocal[5, 0], self.length, self)

        Szx = Szx.combine(fxi.Sz, 1, 1)
        Szx = Szx.combine(fyi.Sz, 1, 1)
        Szx = Szx.combine(mzi.Sz, 1, 1)
        Szx = Szx.combine(fxj.Sz, 1, 1)
        Szx = Szx.combine(fyj.Sz, 1, 1)
        Szx = Szx.combine(mzj.Sz, 1, 1)

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)
                if load_factor != 0:
                    Szx = Szx.combine(load.Sz, 1, load_factor)

        self.Sz[load_combination.name] = Szx

    def generate_DxDy_function(self, load_combination):
        """
        Generate the piecewise displacement functions for the local x and y 
        axis. !!Note the nodal displacements are not included in these functions.

        :param load_combination: load combination element
        :type load_combination: _type_
        """

        if not self._stations:
            self.stations()

        empty_f = np.zeros((6, 1))

        Fendlocal = self.end_forces_local.get(load_combination.name, empty_f)

        # Empty Piecwise functions to build the total function from the loading
        dx = loadtypes.Piecewise_Polynomial()
        dy = loadtypes.Piecewise_Polynomial()

        # Create "loads" from the end forces and combine with dx and dy
        fxi = loadtypes.R2_Axial_Load(Fendlocal[0, 0], 0, self)
        fyi = loadtypes.R2_Point_Load(Fendlocal[1, 0], 0, self)
        mzi = loadtypes.R2_Point_Moment(Fendlocal[2, 0], 0, self)
        fxj = loadtypes.R2_Axial_Load(Fendlocal[3, 0], self.length, self)
        fyj = loadtypes.R2_Point_Load(Fendlocal[4, 0], self.length, self)
        mzj = loadtypes.R2_Point_Moment(Fendlocal[5, 0], self.length, self)

        dx = dx.combine(fxi.Dx, 1, 1)
        dy = dy.combine(fyi.Dy, 1, 1)
        dy = dy.combine(mzi.Dy, 1, 1)
        dx = dx.combine(fxj.Dx, 1, 1)
        dy = dy.combine(fyj.Dy, 1, 1)
        dy = dy.combine(mzj.Dy, 1, 1)

        # Combine Piecewise Deflection Functions of all of the loads
        if self._loaded:
            for load in self.loads:
                load_factor = load_combination.factors.get(load.loadcase, 0)

                if load_factor != 0:
                    dx = dx.combine(load.Dx, 1, load_factor)
                    dy = dy.combine(load.Dy, 1, load_factor)

        self.Dx[load_combination.name] = dx
        self.Dy[load_combination.name] = dy

    def Wxlocal_plot(self, load_combination, scale=1, ptloadscale=1):
        if not self._stations:
            self.stations()

        wx = self.Wx.get(load_combination.name, None)

        if wx is None:
            self.generate_Loading_function(load_combination)
            wx = self.Wx.get(load_combination.name, None)

        wxlocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            w = wx.evaluate(x)

            wp = 0

            for load in self.loads:
                if load.kind == "AXIAL_POINT":
                    if load.a == x:
                        load_factor = load_combination.factors.get(load.loadcase, 0)
                        wp += load_factor * load.p

            wxlocal_span[i, 0] = x
            wxlocal_span[i, 1] = w * scale + (wp * ptloadscale)

        return wxlocal_span

    def Wxglobal_plot(self, load_combination, scale=1, ptloadscale=1):
        wxlocal_plot = self.Wxlocal_plot(
            load_combination, scale=scale, ptloadscale=ptloadscale
        )

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        wxglobal_plot = np.matmul(wxlocal_plot, R)

        return wxglobal_plot

    def Wylocal_plot(self, load_combination, scale=1, ptloadscale=1):
        if not self._stations:
            self.stations()

        wy = self.Wy.get(load_combination.name, None)

        if wy is None:
            self.generate_Loading_function(load_combination)
            wy = self.Wy.get(load_combination.name, None)

        wylocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            w = wy.evaluate(x)

            wp = 0

            for load in self.loads:
                if load.kind == "POINT":
                    if load.a == x:
                        load_factor = load_combination.factors.get(load.loadcase, 0)
                        wp += load_factor * load.p

            wylocal_span[i, 0] = x
            wylocal_span[i, 1] = (w * scale) + (wp * ptloadscale)

        return wylocal_span

    def Wyglobal_plot(self, load_combination, scale=1, ptloadscale=1):
        wylocal_plot = self.Wylocal_plot(
            load_combination, scale=scale, ptloadscale=ptloadscale
        )

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        wyglobal_plot = np.matmul(wylocal_plot, R)

        return wyglobal_plot

    def Alocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        ax = self.A.get(load_combination.name, None)

        if ax is None:
            self.generate_Axial_function(load_combination)
            ax = self.A.get(load_combination.name, None)

        axlocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            a = ax.evaluate(x)

            axlocal_span[i, 0] = x
            axlocal_span[i, 1] = a * scale

        return axlocal_span

    def Aglobal_plot(self, load_combination, scale):
        axlocal_plot = self.Alocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        axglobal_plot = np.matmul(axlocal_plot, R)

        return axglobal_plot

    def Vlocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        vy = self.Vy.get(load_combination.name, None)

        if vy is None:
            self.generate_Vy_function(load_combination)
            vy = self.Vy.get(load_combination.name, None)

        vlocal_span = np.zeros((len(self.calcstations), 2))

        for i, x in enumerate(self.calcstations):
            v = vy.evaluate(x)

            vlocal_span[i, 0] = x
            vlocal_span[i, 1] = v * scale

        return vlocal_span

    def Vglobal_plot(self, load_combination, scale):
        vlocal_plot = self.Vlocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        vglobal_plot = np.matmul(vlocal_plot, R)

        return vglobal_plot

    def Mlocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        mzx = self.Mz.get(load_combination.name, None)

        if mzx is None:
            self.generate_Mz_function(load_combination)
            mzx = self.Mz.get(load_combination.name, None)

        # Get the Roots of the shear function for the current combo
        vy = self.Vy.get(load_combination.name, None)

        if vy is None:
            self.generate_Vy_function(load_combination)
            vy = self.Vy.get(load_combination.name, None)

        shear_roots = vy.roots()
        # Generate a new station list including the roots
        stations = sorted(set(self.calcstations + shear_roots))

        mlocal_span = np.zeros((len(stations), 2))

        for i, x in enumerate(stations):
            m = mzx.evaluate(x)

            mlocal_span[i, 0] = x
            mlocal_span[i, 1] = m * scale

        return mlocal_span

    def Mglobal_plot(self, load_combination, scale):
        mlocal_plot = self.Mlocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        mglobal_plot = np.matmul(mlocal_plot, R)

        return mglobal_plot

    def Slocal_plot(self, load_combination, scale=1):
        if not self._stations:
            self.stations()

        Szx = self.Sz.get(load_combination.name, None)

        if Szx is None:
            self.generate_Sz_function(load_combination)
            Szx = self.Sz.get(load_combination.name, None)

        slocal_span = np.zeros((len(self.calcstations), 2))
        # slope adjustment for end displacements
        Dlocal = self.Dlocal(load_combination)

        sadjust = (Dlocal[0, 4] - Dlocal[0, 1]) / self.length

        for i, x in enumerate(self.calcstations):
            s = Szx.evaluate(x)

            slocal_span[i, 0] = x
            slocal_span[i, 1] = (s + sadjust) * scale

        return slocal_span

    def Sglobal_plot(self, load_combination, scale):
        slocal_plot = self.Slocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        sglobal_plot = np.matmul(slocal_plot, R)

        return sglobal_plot

    def Dlocal_plot(self, load_combination, scale=1):
        dx = self.Dx.get(load_combination.name, None)
        dy = self.Dy.get(load_combination.name, None)

        if dx is None or dy is None:
            self.generate_DxDy_function(load_combination)
            dx = self.Dx.get(load_combination.name, None)
            dy = self.Dy.get(load_combination.name, None)

        Dlocal = self.Dlocal(load_combination)

        # Parametric Functions defining a linear relationship for deflection
        # in each axis based on the Ux and Uy nodal displacements
        Dx = lambda x: Dlocal[0, 0] + (x / self.length) * (Dlocal[0, 3] - Dlocal[0, 0])
        Dy = lambda x: Dlocal[0, 1] + (x / self.length) * (Dlocal[0, 4] - Dlocal[0, 1])

        # Get the Roots of the slope function for the current combo
        sz = self.Sz.get(load_combination.name, None)

        if sz is None:
            self.generate_Sz_function(load_combination)
            sz = self.Sz.get(load_combination.name, None)

        slope_roots = sz.roots()
        # Generate a new station list including the roots
        stations = sorted(set(self.calcstations + slope_roots))

        dlocal_span = np.zeros((len(stations), 2))

        for i, x in enumerate(stations):
            dxl = dx.evaluate(x) + Dx(0)
            dyl = dy.evaluate(x) + Dy(x)

            dlocal_span[i, 0] = x + (dxl * scale)
            dlocal_span[i, 1] = dyl * scale

        return dlocal_span

    def Dglobal_plot(self, load_combination, scale=1):
        dlocal_plot = self.Dlocal_plot(load_combination, scale=scale)

        c = (self.jnode.x - self.inode.x) / self.length
        s = (self.jnode.y - self.inode.y) / self.length

        R = np.matrix([[c, s], [-s, c]])

        dglobal_plot = np.matmul(dlocal_plot, R)

        return dglobal_plot
    
    def Mzextremes(self, load_combination):
        if not self._stations:
            self.stations()

        mzx = self.Mz.get(load_combination.name, None)

        if mzx is None:
            self.generate_Mz_function(load_combination)
            mzx = self.Mz.get(load_combination.name, None)

        # Get the Roots of the shear function for the current combo
        vy = self.Vy.get(load_combination.name, None)

        if vy is None:
            self.generate_Vy_function(load_combination)
            vy = self.Vy.get(load_combination.name, None)

        shear_roots = vy.roots()
        # Generate a new station list including the roots
        stations = sorted(set(self.maxstations + shear_roots))
        maxM = [0,0]
        minM = [0,0]

        for x in stations:
            m = mzx.evaluate(x)
            maxM[1] = max(maxM[1],m)
            minM[1] = min(minM[1],m)
            if maxM[1] == m:
                maxM[0] = x
            if minM[1] == m:
                minM[0] = x
        
        return {"MaxM":maxM,"MinM":minM}