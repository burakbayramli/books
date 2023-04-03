# -*- coding: utf-8 -*-

# © Copyright Charles Vanwynsberghe (2021)

# Pyworld3 is a computer program whose purpose is to run configurable
# simulations of the World3 model as described in the book "Dynamics
# of Growth in a Finite World".

# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software. You can use,
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info".

# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.

# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.

# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

import os
import json

from scipy.interpolate import interp1d
import numpy as np

from .specials import Smooth, clip
from .utils import requires


class Capital:
    """
    Capital sector. Can be run independantly from other sectors with exogenous
    inputs. The initial code is defined p.253.

    Examples
    --------
    Running the capital sector alone requires artificial (exogenous) inputs
    which should be provided by the other sectors. Start from the following
    example:

    >>> cap = Capital()
    >>> cap.set_capital_table_functions()
    >>> cap.init_capital_variables()
    >>> cap.init_capital_constants()
    >>> cap.set_capital_delay_functions()
    >>> cap.init_exogenous_inputs()
    >>> cap.run_capital()

    Attributes
    ----------
    ici : float, optional
        industrial capital initial [dollars]. The default is 2.1e11.
    sci : float, optional
        service capital initial [dollars]. The default is 1.44e11.
    iet : float, optional
        industrial equilibrium time [years]. The default is 4000.
    iopcd : float, optional
        industrial output per capita desired [dollars/person-year]. The
        default is 400.
    lfpf : float, optional
        labor force participation fraction []. The default is 0.75.
    lufdt : float, optional
        labor utilization fraction delay time [years]. The default is 2.
    icor1 : float, optional
        icor, value before time=pyear [years]. The default is 3.
    icor2 : float, optional
        icor, value after time=pyear [years]. The default is 3.
    scor1 : float, optional
        scor, value before time=pyear [years]. The default is 1.
    scor2 : float, optional
        scor, value after time=pyear [years]. The default is 1.
    alic1 : float, optional
        alic, value before time=pyear [years]. The default is 14.
    alic2 : float, optional
        alic, value after time=pyear [years]. The default is 14.
    alsc1 : float, optional
        alsc, value before time=pyear [years]. The default is 20.
    alsc2 : float, optional
        alsc, value after time=pyear [years]. The default is 20.
    fioac1 : float, optional
        fioac, value before time=pyear []. The default is 0.43.
    fioac2 : float, optional
        fioac, value after time=pyear []. The default is 0.43.

    **Industrial subsector**

    ic : numpy.ndarray
        industrial capital [dollars]. It is a state variable.
    io : numpy.ndarray
        industrial output [dollars/year].
    icdr : numpy.ndarray
        industrial capital depreciation rate [dollars/year].
    icir : numpy.ndarray
        industrial capital investment rate [dollars/year].
    icor : numpy.ndarray
        industrial capital-output ratio [years].
    iopc : numpy.ndarray
        industrial output per capita [dollars/person-year].
    alic : numpy.ndarray
        average lifetime of industrial capital [years].
    fioac : numpy.ndarray
        fraction of industrial output allocated to consumption [].
    fioacc : numpy.ndarray
        fioac constant [].
    fioacv : numpy.ndarray
        fioac variable [].
    fioai : numpy.ndarray
        fraction of industrial output allocated to industry [].

    **Service subsector**

    sc : numpy.ndarray
        service capital [dollars]. It is a state variable.
    so : numpy.ndarray
        service output [dollars/year].
    scdr : numpy.ndarray
        service capital depreciation rate [dollars/year].
    scir : numpy.ndarray
        service capital investment rate [dollars/year].
    scor : numpy.ndarray
        service capital-output ratio [years].
    sopc : numpy.ndarray
        service output per capita [dollars/person-year].
    alsc : numpy.ndarray
        average lifetime of service capital [years].
    isopc : numpy.ndarray
        indicated service output per capita [dollars/person-year].
    isopc1 : numpy.ndarray
        isopc, value before time=pyear [dollars/person-year].
    isopc2 : numpy.ndarray
        isopc, value after time=pyear [dollars/person-year].
    fioas : numpy.ndarray
        fraction of industrial output allocated to services [].
    fioas1 : numpy.ndarray
        fioas, value before time=pyear [].
    fioas2 : numpy.ndarray
        fioas, value after time=pyear [].

    **Job subsector**

    j : numpy.ndarray
        jobs [persons].
    jph : numpy.ndarray
        jobs per hectare [persons/hectare].
    jpicu : numpy.ndarray
        jobs per industrial capital unit [persons/dollar].
    jpscu : numpy.ndarray
        jobs per service capital unit [persons/dollar].
    lf : numpy.ndarray
        labor force [persons].
    cuf : numpy.ndarray
        capital utilization fraction [].
    luf : numpy.ndarray
        labor utilization fraction [].
    lufd : numpy.ndarray
        labor utilization fraction delayed [].
    pjas : numpy.ndarray
        potential jobs in agricultural sector [persons].
    pjis : numpy.ndarray
        potential jobs in industrial sector [persons].
    pjss : numpy.ndarray
        potential jobs in service sector [persons].

    """

    def __init__(self, year_min=1900, year_max=2000, dt=1, pyear=1975,
                 verbose=False):
        self.pyear = pyear
        self.dt = dt
        self.year_min = year_min
        self.year_max = year_max
        self.length = self.year_max - self.year_min
        self.n = int(self.length / self.dt)
        self.time = np.arange(self.year_min, self.year_max, self.dt)
        self.verbose = False

    def init_capital_constants(self, ici=2.1e11, sci=1.44e11, iet=4000,
                               iopcd=400, lfpf=0.75, lufdt=2, icor1=3, icor2=3,
                               scor1=1, scor2=1, alic1=14, alic2=14,
                               alsc1=20, alsc2=20, fioac1=0.43, fioac2=0.43):
        """
        Initialize the constant parameters of the capital sector. Constants
        and their unit are documented above at the class level.

        """
        self.ici = ici
        self.sci = sci
        self.iet = iet
        self.iopcd = iopcd
        self.lfpf = lfpf
        self.lufdt = lufdt
        self.icor1 = icor1
        self.icor2 = icor2
        self.scor1 = scor1
        self.scor2 = scor2
        self.alic1 = alic1
        self.alic2 = alic2
        self.alsc1 = alsc1
        self.alsc2 = alsc2
        self.fioac1 = fioac1
        self.fioac2 = fioac2

    def init_capital_variables(self):
        """
        Initialize the state and rate variables of the capital sector
        (memory allocation). Variables and their unit are documented above at
        the class level.

        """
        # industrial sector
        self.ic = np.full((self.n,), np.nan)
        self.io = np.full((self.n,), np.nan)
        self.icdr = np.full((self.n,), np.nan)
        self.icir = np.full((self.n,), np.nan)
        self.icor = np.full((self.n,), np.nan)
        self.iopc = np.full((self.n,), np.nan)
        self.alic = np.full((self.n,), np.nan)
        self.fioac = np.full((self.n,), np.nan)
        self.fioacc = np.full((self.n,), np.nan)
        self.fioai = np.full((self.n,), np.nan)
        self.fioacv = np.full((self.n,), np.nan)
        # service subsector
        self.sc = np.full((self.n,), np.nan)
        self.so = np.full((self.n,), np.nan)
        self.scdr = np.full((self.n,), np.nan)
        self.scir = np.full((self.n,), np.nan)
        self.scor = np.full((self.n,), np.nan)
        self.sopc = np.full((self.n,), np.nan)
        self.alsc = np.full((self.n,), np.nan)
        self.isopc = np.full((self.n,), np.nan)
        self.isopc1 = np.full((self.n,), np.nan)
        self.isopc2 = np.full((self.n,), np.nan)
        self.fioas = np.full((self.n,), np.nan)
        self.fioas1 = np.full((self.n,), np.nan)
        self.fioas2 = np.full((self.n,), np.nan)
        # job subsector
        self.cuf = np.full((self.n,), np.nan)
        self.j = np.full((self.n,), np.nan)
        self.jph = np.full((self.n,), np.nan)
        self.jpicu = np.full((self.n,), np.nan)
        self.jpscu = np.full((self.n,), np.nan)
        self.lf = np.full((self.n,), np.nan)
        self.luf = np.full((self.n,), np.nan)
        self.lufd = np.full((self.n,), np.nan)
        self.pjas = np.full((self.n,), np.nan)
        self.pjis = np.full((self.n,), np.nan)
        self.pjss = np.full((self.n,), np.nan)

    def set_capital_delay_functions(self, method="euler"):
        """
        Set the linear smoothing and delay functions of the 1st or the 3rd
        order, for the capital sector. One should call
        `self.set_capital_delay_functions` after calling
        `self.init_capital_constants`.

        Parameters
        ----------
        method : str, optional
            Numerical integration method: "euler" or "odeint". The default is
            "euler".

        """
        var_smooth = ["LUF"]
        for var_ in var_smooth:
            func_delay = Smooth(getattr(self, var_.lower()),
                                self.dt, self.time, method=method)
            setattr(self, "smooth_"+var_.lower(), func_delay)

    def set_capital_table_functions(self, json_file=None):
        """
        Set the nonlinear functions of the capital sector, based on a json
        file. By default, the `functions_table_world3.json` file from pyworld3
        is used.

        Parameters
        ----------
        json_file : file, optional
            json file containing all tables. The default is None.

        """
        if json_file is None:
            json_file = "./functions_table_world3.json"
            json_file = os.path.join(os.path.dirname(__file__), json_file)
        with open(json_file) as fjson:
            tables = json.load(fjson)

        func_names = ["FIOACV", "ISOPC1", "ISOPC2", "FIOAS1", "FIOAS2",
                      "JPICU", "JPSCU", "JPH", "CUF"]

        for func_name in func_names:
            for table in tables:
                if table["y.name"] == func_name:
                    func = interp1d(table["x.values"], table["y.values"],
                                    bounds_error=False,
                                    fill_value=(table["y.values"][0],
                                                table["y.values"][-1]))
                    setattr(self, func_name.lower()+"_f", func)

    def init_exogenous_inputs(self):
        """
        Initialize all the necessary constants and variables to run the
        capital sector alone. These exogenous parameters are outputs from
        the 4 other remaining sectors in a full simulation of World3.

        """
        # variables
        # agriculture input per hectare
        self.aiph = np.full((self.n,), np.nan)
        # arable land
        self.al = np.full((self.n,), np.nan)
        # population sector
        self.pop = np.full((self.n,), np.nan)
        self.p2 = np.full((self.n,), np.nan)
        self.p3 = np.full((self.n,), np.nan)
        # capital to resource
        self.fcaor = np.full((self.n,), np.nan)
        # capital to agriculture
        self.fioaa = np.full((self.n,), np.nan)
        # tables
        func_names = ["AIPH", "AL", "POP", "FCAOR", "FIOAA"]
        y_values = [[5., 11., 21., 34., 58., 86., 123., 61., 23., 8., 3.],
                    [_ * 10**8 for _ in [9., 10., 11., 13., 16., 20., 23.,
                                         24., 24., 24., 24.]],
                    [_ * 10**9 for _ in [1.65, 1.73, 1.8, 2.1, 2.3, 2.55, 3.,
                                         3.65, 4., 4.6, 5.15]],
                    11*[.05],
                    11*[.1]]
        x_to_2100 = np.linspace(1900, 2100, 11)
        x_to_2000 = np.linspace(1900, 2000, 11)
        x_values = [x_to_2100, x_to_2100, x_to_2000, x_to_2000, x_to_2000]

        for func_name, x_vals, y_vals in zip(func_names, x_values, y_values):
            func = interp1d(x_vals, y_vals,
                            bounds_error=False,
                            fill_value=(y_vals[0],
                                        y_vals[-1]))
            setattr(self, func_name.lower()+"_f", func)

    def loopk_exogenous(self, k):
        """
        Run a sorted sequence to update one loop of the exogenous parameters.
        `@requires` decorator checks that all dependencies are computed
        previously.

        """
        self.aiph[k] = self.aiph_f(self.time[k])
        self.al[k] = self.al_f(self.time[k])
        self.pop[k] = self.pop_f(self.time[k])
        self.p2[k] = 0.25 * self.pop[k]
        self.p3[k] = 0.25 * self.pop[k]
        self.fcaor[k] = self.fcaor_f(self.time[k])
        self.fioaa[k] = self.fioaa_f(self.time[k])

    def loop0_exogenous(self):
        """
        Run a sequence to initialize the exogenous parameters (loop with k=0).

        """
        self.loopk_exogenous(0)

    def loop0_capital(self, alone=False):
        """
        Run a sequence to initialize the capital sector (loop with k=0).

        Parameters
        ----------
        alone : boolean, optional
            if True, run the sector alone with exogenous inputs. The default
            is False.

        """
        if alone:
            self.loop0_exogenous()
        # Set initial conditions
        self.ic[0] = self.ici
        self.sc[0] = self.sci
        self.cuf[0] = 1.
        # industrial subsector
        self._update_alic(0)
        self._update_icdr(0, 0)
        self._update_icor(0)
        self._update_io(0)
        self._update_iopc(0)
        self._update_fioac(0)
        # service subsector
        self._update_isopc(0)
        self._update_alsc(0)
        self._update_scdr(0, 0)
        self._update_scor(0)
        self._update_so(0)
        self._update_sopc(0)
        self._update_fioas(0)
        self._update_scir(0, 0)
        # back to industrial sector
        self._update_fioai(0)
        self._update_icir(0, 0)
        # back to job subsector
        self._update_jpicu(0)
        self._update_pjis(0)
        self._update_jpscu(0)
        self._update_pjss(0)
        self._update_jph(0)
        self._update_pjas(0)
        self._update_j(0)
        self._update_lf(0)
        self._update_luf(0)
        self._update_lufd(0)
        # recompute supplementary initial conditions
        self._update_cuf(0)

    def loopk_capital(self, j, k, jk, kl, alone=False):
        """
        Run a sequence to update one loop of the capital sector.

        Parameters
        ----------
        alone : boolean, optional
            if True, run the sector alone with exogenous inputs. The default
            is False.

        """
        if alone:
            self.loopk_exogenous(k)
        # job subsector
        self._update_lufd(k)
        self._update_cuf(k)
        # industrial subsector
        self._update_state_ic(k, j, jk)
        self._update_alic(k)
        self._update_icdr(k, kl)
        self._update_icor(k)
        self._update_io(k)
        self._update_iopc(k)
        self._update_fioac(k)
        # service subsector
        self._update_state_sc(k, j, jk)
        self._update_isopc(k)
        self._update_alsc(k)
        self._update_scdr(k, kl)
        self._update_scor(k)
        self._update_so(k)
        self._update_sopc(k)
        self._update_fioas(k)
        self._update_scir(k, kl)
        # back to industrial sector
        self._update_fioai(k)
        self._update_icir(k, kl)
        # back to job subsector
        self._update_jpicu(k)
        self._update_pjis(k)
        self._update_jpscu(k)
        self._update_pjss(k)
        self._update_jph(k)
        self._update_pjas(k)
        self._update_j(k)
        self._update_lf(k)
        self._update_luf(k)

    def run_capital(self):
        """
        Run a sequence of updates to simulate the capital sector alone with
        exogenous inputs.

        """
        self.redo_loop = True
        while self.redo_loop:
            self.redo_loop = False
            self.loop0_capital(alone=True)
        for k_ in range(1, self.n):
            self.redo_loop = True
            while self.redo_loop:
                self.redo_loop = False
                if self.verbose:
                    print("go loop", k_)
                self.loopk_capital(k_-1, k_, k_-1, k_, alone=True)

    @requires(["lufd"], ["luf"], check_after_init=False)
    def _update_lufd(self, k):
        """
        From step k=0 requires: LUF, else nothing
        """
        self.lufd[k] = self.smooth_luf(k, self.lufdt)

    @requires(["cuf"], ["lufd"])
    def _update_cuf(self, k):
        """
        From step k requires: LUFD
        """
        self.cuf[k] = self.cuf_f(self.lufd[k])

    @requires(["ic"])
    def _update_state_ic(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        if k == 0:
            self.ic[k] = self.ici
        else:
            self.ic[k] = self.ic[j] + self.dt * (self.icir[jk] - self.icdr[jk])

    @requires(["alic"])
    def _update_alic(self, k):
        """
        From step k requires: nothing
        """
        self.alic[k] = clip(self.alic2, self.alic1, self.time[k], self.pyear)

    @requires(["icdr"], ["ic", "alic"])
    def _update_icdr(self, k, kl):
        """
        From step k requires: IC ALIC
        """
        self.icdr[kl] = self.ic[k] / self.alic[k]

    @requires(["icor"])
    def _update_icor(self, k):
        """
        From step k requires: nothing
        """
        self.icor[k] = clip(self.icor2, self.icor1, self.time[k], self.pyear)

    @requires(["io"], ["ic", "fcaor", "cuf", "icor"])
    def _update_io(self, k):
        """
        From step k requires: IC FCAOR CUF ICOR
        """
        self.io[k] = (self.ic[k] * (1 - self.fcaor[k]) * self.cuf[k] /
                      self.icor[k])

    @requires(["iopc"], ["io", "pop"])
    def _update_iopc(self, k):
        """
        From step k requires: IO POP
        """
        self.iopc[k] = self.io[k] / self.pop[k]

    @requires(["fioacv", "fioacc", "fioac"], ["iopc"])
    def _update_fioac(self, k):
        """
        From step k requires: IOPC
        """
        self.fioacv[k] = self.fioacv_f(self.iopc[k] / self.iopcd)
        self.fioacc[k] = clip(self.fioac2, self.fioac1, self.time[k],
                              self.pyear)
        self.fioac[k] = clip(self.fioacv[k], self.fioacc[k], self.time[k],
                             self.iet)

    @requires(["sc"])
    def _update_state_sc(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        if k == 0:
            self.sc[k] = self.sci
        else:
            self.sc[k] = self.sc[j] + self.dt * (self.scir[jk] - self.scdr[jk])

    @requires(["isopc1", "isopc2", "isopc"], ["iopc"])
    def _update_isopc(self, k):
        """
        From step k requires: IOPC
        """
        self.isopc1[k] = self.isopc1_f(self.iopc[k])
        self.isopc2[k] = self.isopc2_f(self.iopc[k])
        self.isopc[k] = clip(self.isopc2[k], self.isopc1[k], self.time[k],
                             self.pyear)

    @requires(["alsc"])
    def _update_alsc(self, k):
        """
        From step k requires: nothing
        """
        self.alsc[k] = clip(self.alsc2, self.alsc1, self.time[k], self.pyear)

    @requires(["scdr"], ["sc", "alsc"])
    def _update_scdr(self, k, kl):
        """
        From step k requires: SC ALSC
        """
        self.scdr[kl] = self.sc[k] / self.alsc[k]

    @requires(["scor"])
    def _update_scor(self, k):
        """
        From step k requires: nothing
        """
        self.scor[k] = clip(self.scor2, self.scor1, self.time[k], self.pyear)

    @requires(["so"], ["sc", "cuf", "scor"])
    def _update_so(self, k):
        """
        From step k requires: SC CUF SCOR
        """
        self.so[k] = self.sc[k] * self.cuf[k] / self.scor[k]

    @requires(["sopc"], ["so", "pop"])
    def _update_sopc(self, k):
        """
        From step k requires: SO POP
        """
        self.sopc[k] = self.so[k] / self.pop[k]

    @requires(["fioas1", "fioas2", "fioas"], ["sopc", "isopc"])
    def _update_fioas(self, k):
        """
        From step k requires: SOPC ISOPC
        """
        self.fioas1[k] = self.fioas1_f(self.sopc[k] / self.isopc[k])
        self.fioas2[k] = self.fioas2_f(self.sopc[k] / self.isopc[k])
        self.fioas[k] = clip(self.fioas2[k], self.fioas1[k], self.time[k],
                             self.pyear)

    @requires(["scir"], ["io", "fioas"])
    def _update_scir(self, k, kl):
        """
        From step k requires: IO FIOAS
        """
        self.scir[kl] = self.io[k] * self.fioas[k]

    @requires(["fioai"], ["fioaa", "fioas", "fioac"])
    def _update_fioai(self, k):
        """
        From step k requires: FIOAA FIOAS FIOAC
        """
        self.fioai[k] = (1 - self.fioaa[k] - self.fioas[k] - self.fioac[k])

    @requires(["icir"], ["io", "fioai"])
    def _update_icir(self, k, kl):
        """
        From step k requires: IO FIOAI
        """
        self.icir[kl] = self.io[k] * self.fioai[k]

    @requires(["jpicu"], ["iopc"])
    def _update_jpicu(self, k):
        """
        From step k requires: IOPC
        """
        self.jpicu[k] = self.jpicu_f(self.iopc[k])

    @requires(["pjis"], ["ic", "jpicu"])
    def _update_pjis(self, k):
        """
        From step k requires: IC JPICU
        """
        self.pjis[k] = self.ic[k] * self.jpicu[k]

    @requires(["jpscu"], ["sopc"])
    def _update_jpscu(self, k):
        """
        From step k requires: SOPC
        """
        self.jpscu[k] = self.jpscu_f(self.sopc[k])

    @requires(["pjss"], ["sc", "jpscu"])
    def _update_pjss(self, k):
        """
        From step k requires: SC JPSCU
        """
        self.pjss[k] = self.sc[k] * self.jpscu[k]

    @requires(["jph"], ["aiph"])
    def _update_jph(self, k):
        """
        From step k requires: AIPH
        """
        self.jph[k] = self.jph_f(self.aiph[k])

    @requires(["pjas"], ["jph", "al"])
    def _update_pjas(self, k):
        """
        From step k requires: JPH AL
        """
        self.pjas[k] = self.jph[k] * self.al[k]

    @requires(["j"], ["pjis", "pjas", "pjss"])
    def _update_j(self, k):
        """
        From step k requires: PJIS PJAS PJSS
        """
        self.j[k] = self.pjis[k] + self.pjas[k] + self.pjss[k]

    @requires(["lf"], ["p2", "p3"])
    def _update_lf(self, k):
        """
        From step k requires: P2 P3
        """
        self.lf[k] = (self.p2[k] + self.p3[k]) * self.lfpf

    @requires(["luf"], ["j", "lf"])
    def _update_luf(self, k):
        """
        From step k requires: J LF
        """
        self.luf[k] = self.j[k] / self.lf[k]
