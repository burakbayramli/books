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

from .specials import Dlinf3, Smooth, clip, ramp
from .utils import requires


class Population:
    """
    Population sector with four age levels. Can be run independantly from other
    sectors with exogenous inputs. The initial code is defined p.170.

    Examples
    --------
    Running the population sector alone requires artificial (exogenous) inputs
    which should be provided by the other sectors. Start from the following
    example:

    >>> pop = Population()
    >>> pop.set_population_table_functions()
    >>> pop.init_population_constants()
    >>> pop.init_population_variables()
    >>> pop.init_exogenous_inputs()
    >>> pop.set_population_delay_functions()
    >>> pop.run_population()

    Parameters
    ----------
    year_min : float, optional
        start year of the simulation [year]. The default is 0.
    year_max : float, optional
        end year of the simulation [year]. The default is 75.
    dt : float, optional
        time step of the simulation [year]. The default is 1.
    iphst : float, optional
        implementation date of new policy on health service time [year].
        The default is 1940.
    verbose : bool, optional
        print information for debugging. The default is False.

    Attributes
    ----------
    p1i : float, optional
        p2 initial [persons]. The default is 65e7.
    p2i : float, optional
        p2 initial [persons]. The default is 70e7.
    p3i : float, optional
        p3 initial [persons]. The default is 19e7.
    p4i : float, optional
        p4 initial [persons]. The default is 6e7.
    dcfsn : float, optional
        desired completed family size normal []. The default is 4.
    fcest : float, optional
        fertility control effectiveness set time [year]. The default is 4000.
    hsid : float, optional
        health services impact delay [years]. The default is 20.
    ieat : float, optional
        income expectation averaging time [years]. The default is 3.
    len : float, optional
        life expectancy normal [years]. The default is 28.
    lpd : float, optional
        lifetime perception delay [years]. The default is 20.
    mtfn : float, optional
        maximum total fertility normal []. The default is 12.
    pet : float, optional
        population equilibrium time [year]. The default is 4000.
    rlt : float, optional
        reproductive lifetime [years]. The default is 30.
    sad : float, optional
        social adjustment delay [years]. The default is 20.
    zpgt : float, optional
        time when desired family size equals 2 children [year]. The default is
        4000.

    **Population sector**

    p1 : numpy.ndarray
        population, ages 0-14 [persons]. It is a state variable.
    p2 : numpy.ndarray
        population, ages 15-44 [persons]. It is a state variable.
    p3 : numpy.ndarray
        population, ages 45-64 [persons]. It is a state variable.
    p4 : numpy.ndarray
        population, ages 65+ [persons]. It is a state variable.
    pop : numpy.ndarray
        population [persons].
    mat1 : numpy.ndarray
        maturation rate, age 14-15 [persons/year].
    mat2 : numpy.ndarray
        maturation rate, age 44-45 [persons/year].
    mat3 : numpy.ndarray
        maturation rate, age 64-65 [persons/year].

    **Death rate subsector**

    d : numpy.ndarray
        deaths per year [persons/year].
    d1 : numpy.ndarray
        deaths per year, ages 0-14 [persons/year].
    d2 : numpy.ndarray
        deaths per year, ages 15-44 [persons/year].
    d3 : numpy.ndarray
        deaths per year, ages 45-64 [persons/year].
    d4 : numpy.ndarray
        deaths per year, ages 65+ [persons/year].
    cdr : numpy.ndarray
        crude death rate [deaths/1000 person-years].
    ehspc : numpy.ndarray
        effective health services per capita [dollars/person-year].
    fpu : numpy.ndarray
        fraction of population urban [].
    hsapc : numpy.ndarray
        health services allocations per capita [dollars/person-year].
    le : numpy.ndarray
        life expectancy [years].
    lmc : numpy.ndarray
        lifetime multiplier from crowding [].
    lmf : numpy.ndarray
        lifetime multiplier from food [].
    lmhs : numpy.ndarray
        lifetime multiplier from health services [].
    lmhs1 : numpy.ndarray
        lmhs, value before time=pyear [].
    lmhs2 : numpy.ndarray
        lmhs, value after time=pyear [].
    lmp : numpy.ndarray
        lifetime multiplier from persistent pollution [].
    m1 : numpy.ndarray
        mortality, ages 0-14 [deaths/person-year].
    m2 : numpy.ndarray
        mortality, ages 15-44 [deaths/person-year].
    m3 : numpy.ndarray
        mortality, ages 45-64 [deaths/person-year].
    m4 : numpy.ndarray
        mortality, ages 65+ [deaths/person-year].

    **Birth rate subsector**

    b : numpy.ndarray
        births per year [persons/year].
    aiopc : numpy.ndarray
        average industrial output per capita [dollars/person-year].
    cbr : numpy.ndarray
        crude birth rate [births/1000 person-years].
    cmi : numpy.ndarray
        crowding multiplier from industrialization [].
    cmple : numpy.ndarray
        compensatory multiplier from perceived life expectancy [].
    diopc : numpy.ndarray
        delayed industrial output per capita [dollars/person-year].
    dtf : numpy.ndarray
        desired total fertility [].
    dcfs : numpy.ndarray
        desired completed family size [].
    fcapc : numpy.ndarray
        fertility control allocations per capita [dollars/person-year].
    fce : numpy.ndarray
        fertility control effectiveness [].
    fcfpc : numpy.ndarray
        fertility control facilities per capita [dollars/person-year].
    fie : numpy.ndarray
        family income expectation [].
    fm : numpy.ndarray
        fecundity multiplier [].
    frsn : numpy.ndarray
        family response to social norm [].
    fsafc : numpy.ndarray
        fraction of services allocated to fertility control [].
    mtf : numpy.ndarray
        maximum total fertility [].
    nfc : numpy.ndarray
        need for fertility control [].
    ple : numpy.ndarray
        perceived life expectancy [years].
    sfsn : numpy.ndarray
        social family size norm [].
    tf : numpy.ndarray
        total fertility [].

    """

    def __init__(self, year_min=1900, year_max=1975, dt=1, iphst=1940,
                 verbose=False):
        self.iphst = iphst
        self.dt = dt
        self.year_min = year_min
        self.year_max = year_max
        self.verbose = verbose
        self.length = self.year_max - self.year_min
        self.n = int(self.length / self.dt)
        self.time = np.arange(self.year_min, self.year_max, self.dt)

    def init_population_constants(self, p1i=65e7, p2i=70e7, p3i=19e7, p4i=6e7,
                                  dcfsn=4, fcest=4000, hsid=20, ieat=3, len=28,
                                  lpd=20, mtfn=12, pet=4000, rlt=30, sad=20,
                                  zpgt=4000):
        """
        Initialize the constant parameters of the population sector. Constants
        and their unit are documented above at the class level.

        """
        self.p1i = p1i
        self.p2i = p2i
        self.p3i = p3i
        self.p4i = p4i
        self.dcfsn = dcfsn
        self.fcest = fcest
        self.hsid = hsid
        self.ieat = ieat
        self.len = len
        self.lpd = lpd
        self.mtfn = mtfn
        self.pet = pet
        self.rlt = rlt
        self.sad = sad
        self.zpgt = zpgt

    def init_population_variables(self):
        """
        Initialize the state and rate variables of the population sector
        (memory allocation). Variables and their unit are documented above at
        the class level.

        """
        # population sector
        self.pop = np.full((self.n,), np.nan)
        self.p1 = np.full((self.n,), np.nan)
        self.p2 = np.full((self.n,), np.nan)
        self.p3 = np.full((self.n,), np.nan)
        self.p4 = np.full((self.n,), np.nan)
        self.d1 = np.full((self.n,), np.nan)
        self.d2 = np.full((self.n,), np.nan)
        self.d3 = np.full((self.n,), np.nan)
        self.d4 = np.full((self.n,), np.nan)
        self.mat1 = np.full((self.n,), np.nan)
        self.mat2 = np.full((self.n,), np.nan)
        self.mat3 = np.full((self.n,), np.nan)
        # death rate subsector
        self.d = np.full((self.n,), np.nan)
        self.cdr = np.full((self.n,), np.nan)
        self.fpu = np.full((self.n,), np.nan)
        self.le = np.full((self.n,), np.nan)
        self.lmc = np.full((self.n,), np.nan)
        self.lmf = np.full((self.n,), np.nan)
        self.lmhs = np.full((self.n,), np.nan)
        self.lmhs1 = np.full((self.n,), np.nan)
        self.lmhs2 = np.full((self.n,), np.nan)
        self.lmp = np.full((self.n,), np.nan)
        self.m1 = np.full((self.n,), np.nan)
        self.m2 = np.full((self.n,), np.nan)
        self.m3 = np.full((self.n,), np.nan)
        self.m4 = np.full((self.n,), np.nan)
        self.ehspc = np.full((self.n,), np.nan)
        self.hsapc = np.full((self.n,), np.nan)
        # birth rate subsector
        self.b = np.full((self.n,), np.nan)
        self.cbr = np.full((self.n,), np.nan)
        self.cmi = np.full((self.n,), np.nan)
        self.cmple = np.full((self.n,), np.nan)
        self.tf = np.full((self.n,), np.nan)
        self.dtf = np.full((self.n,), np.nan)
        self.dcfs = np.full((self.n,), np.nan)
        self.fce = np.full((self.n,), np.nan)
        self.fie = np.full((self.n,), np.nan)
        self.fm = np.full((self.n,), np.nan)
        self.frsn = np.full((self.n,), np.nan)
        self.mtf = np.full((self.n,), np.nan)
        self.nfc = np.full((self.n,), np.nan)
        self.ple = np.full((self.n,), np.nan)
        self.sfsn = np.full((self.n,), np.nan)
        self.aiopc = np.full((self.n,), np.nan)
        self.diopc = np.full((self.n,), np.nan)
        self.fcapc = np.full((self.n,), np.nan)
        self.fcfpc = np.full((self.n,), np.nan)
        self.fsafc = np.full((self.n,), np.nan)

    def set_population_delay_functions(self, method="euler"):
        """
        Set the linear smoothing and delay functions of the 1st or the 3rd
        order, for the population sector. One should call
        `self.set_population_delay_functions` after calling
        `self.init_population_constants`.

        Parameters
        ----------
        method : str, optional
            Numerical integration method: "euler" or "odeint". The default is
            "euler".

        """
        var_dlinf3 = ["LE", "IOPC", "FCAPC"]
        for var_ in var_dlinf3:
            func_delay = Dlinf3(getattr(self, var_.lower()),
                                self.dt, self.time, method=method)
            setattr(self, "dlinf3_"+var_.lower(), func_delay)

        var_smooth = ["HSAPC", "IOPC"]
        for var_ in var_smooth:
            func_delay = Smooth(getattr(self, var_.lower()),
                                self.dt, self.time, method=method)
            setattr(self, "smooth_"+var_.lower(), func_delay)

    def set_population_table_functions(self, json_file=None):
        """
        Set the nonlinear functions of the population sector, based on a json
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

        func_names = ["M1", "M2", "M3", "M4",
                      "LMF", "HSAPC", "LMHS1", "LMHS2",
                      "FPU", "CMI", "LMP", "FM", "CMPLE",
                      "SFSN", "FRSN", "FCE_TOCLIP", "FSAFC"]

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
        population sector alone. These exogenous parameters are outputs from
        the 4 other remaining sectors in a full simulation of World3.

        """
        # Constants
        # industrial output
        self.lt = self.year_min + 500
        self.lt2 = self.year_min + 500
        self.cio = 100
        # index of persistent pollution
        self.ps = 0
        self.pt = self.year_min + 10
        # service output
        self.cso = 150
        # food
        self.cfood = 250
        self.sfpc = 230
        # Variables
        # industrial output
        self.io = np.full((self.n,), np.nan)
        self.io1 = np.full((self.n,), np.nan)
        self.io11 = np.full((self.n,), np.nan)
        self.io12 = np.full((self.n,), np.nan)
        self.io2 = np.full((self.n,), np.nan)
        self.iopc = np.full((self.n,), np.nan)
        # index of persistent pollution
        self.ppolx = np.full((self.n,), np.nan)
        # service output
        self.so = np.full((self.n,), np.nan)
        self.so1 = np.full((self.n,), np.nan)
        self.so11 = np.full((self.n,), np.nan)
        self.so12 = np.full((self.n,), np.nan)
        self.so2 = np.full((self.n,), np.nan)
        self.sopc = np.full((self.n,), np.nan)
        # food
        self.f = np.full((self.n,), np.nan)
        self.f1 = np.full((self.n,), np.nan)
        self.f11 = np.full((self.n,), np.nan)
        self.f12 = np.full((self.n,), np.nan)
        self.f2 = np.full((self.n,), np.nan)
        self.fpc = np.full((self.n,), np.nan)

    @requires(["io", "iopc", "ppolx", "so", "sopc", "f", "fpc"], ["pop"])
    def loopk_exogenous(self, k):
        """
        Run a sorted sequence to update one loop of the exogenous parameters.
        `@requires` decorator checks that all dependencies are computed
        previously.

        """
        # industrial output
        self.io11[k] = .7e11*np.exp((self.time[k] - self.year_min)*.037)
        self.io12[k] = self.pop[k] * self.cio
        self.io1[k] = clip(self.io12[k], self.io11[k], self.time[k], self.lt2)
        self.io2[k] = .7e11 * np.exp(self.lt * .037)
        self.io[k] = clip(self.io2[k], self.io1[k], self.time[k], self.lt)
        self.iopc[k] = self.io[k] / self.pop[k]
        # index of persistent pollution
        self.ppolx[k] = 1 + ramp(self.ps, self.pt, self.time[k])
        # service output
        self.so11[k] = 1.5e11 * np.exp((self.time[k] - self.year_min) * .030)
        self.so12[k] = self.pop[k] * self.cso
        self.so1[k] = clip(self.so12[k], self.so11[k], self.time[k], self.lt2)
        self.so2[k] = 1.5e11 * np.exp(self.lt * .030)
        self.so[k] = clip(self.so2[k], self.so1[k], self.time[k], self.lt)
        self.sopc[k] = self.so[k] / self.pop[k]
        # food
        self.f11[k] = 4e11 * np.exp((self.time[k] - self.year_min) * .020)
        self.f12[k] = self.pop[k] * self.cfood
        self.f1[k] = clip(self.f12[k], self.f11[k], self.time[k], self.lt2)
        self.f2[k] = 4e11 * np.exp(self.lt * .020)
        self.f[k] = clip(self.f2[k], self.f1[k], self.time[k], self.lt)
        self.fpc[k] = self.f[k] / self.pop[k]

    def loop0_exogenous(self):
        """
        Run a sequence to initialize the exogenous parameters (loop with k=0).

        """
        self.loopk_exogenous(0)

    def loop0_population(self, alone=False):
        """
        Run a sequence to initialize the population sector (loop with k=0).

        Parameters
        ----------
        alone : boolean, optional
            if True, run the sector alone with exogenous inputs. The default
            is False.

        """
        # Set initial conditions
        self.p1[0] = self.p1i
        self.p2[0] = self.p2i
        self.p3[0] = self.p3i
        self.p4[0] = self.p4i
        self.frsn[0] = 0.82
        self.pop[0] = self.p1[0] + self.p2[0] + self.p3[0] + self.p4[0]
        if alone:
            self.loop0_exogenous()
        # Death rate subsector
        # connect World3 sectors to Population
        # pop from initialisation
        self._update_fpu(0)
        self._update_lmp(0)
        self._update_lmf(0)
        self._update_cmi(0)
        self._update_hsapc(0)
        # inside Population sector
        self._update_ehspc(0)
        self._update_lmhs(0)
        self._update_lmc(0)
        self._update_le(0)
        self._update_m1(0)
        self._update_m2(0)
        self._update_m3(0)
        self._update_m4(0)
        self._update_mat1(0, 0)
        self._update_mat2(0, 0)
        self._update_mat3(0, 0)
        self._update_d1(0, 0)
        self._update_d2(0, 0)
        self._update_d3(0, 0)
        self._update_d4(0, 0)
        self._update_d(0, 0)   # replace (0, -1) by (0, 0) at init
        self._update_cdr(0)
        # Birth rate subsector
        # connect World3 sectors to Population
        # industrial Output > Population
        self._update_aiopc(0)
        self._update_diopc(0)
        self._update_fie(0)
        # inside Population sector
        self._update_sfsn(0)
        self._update_frsn(0)
        self._update_dcfs(0)
        self._update_ple(0)
        self._update_cmple(0)
        self._update_dtf(0)
        self._update_fm(0)
        self._update_mtf(0)
        self._update_nfc(0)
        self._update_fsafc(0)
        self._update_fcapc(0)
        self._update_fcfpc(0)
        self._update_fce(0)
        self._update_tf(0)
        self._update_cbr(0, 0)  # replace (0, -1) by (0, 0) at init
        self._update_b(0, 0)
        # recompute supplementary initial conditions
        self._update_frsn(0)

    def loopk_population(self, j, k, jk, kl, alone=False):
        """
        Run a sequence to update one loop of the population sector.

        Parameters
        ----------
        alone : boolean, optional
            if True, run the sector alone with exogenous inputs. The default
            is False.

        """
        self._update_state_p1(k, j, jk)
        self._update_state_p2(k, j, jk)
        self._update_state_p3(k, j, jk)
        self._update_state_p4(k, j, jk)
        self._update_pop(k)
        if alone:
            self.loopk_exogenous(k)
        # Death rate subsector
        # connect World3 sectors to Population
        self._update_fpu(k)
        self._update_lmp(k)
        self._update_lmf(k)
        self._update_cmi(k)
        self._update_hsapc(k)
        # inside Population sector
        self._update_ehspc(k)
        self._update_lmhs(k)
        self._update_lmc(k)
        self._update_le(k)
        self._update_m1(k)
        self._update_m2(k)
        self._update_m3(k)
        self._update_m4(k)
        self._update_mat1(k, kl)
        self._update_mat2(k, kl)
        self._update_mat3(k, kl)
        self._update_d1(k, kl)
        self._update_d2(k, kl)
        self._update_d3(k, kl)
        self._update_d4(k, kl)
        self._update_d(k, jk)
        self._update_cdr(k)
        # Birth rate subsector
        # connect World3 sectors to Population
        # industrial Output > Population
        self._update_aiopc(k)
        self._update_diopc(k)
        self._update_fie(k)
        # inside Population sector
        self._update_sfsn(k)
        self._update_frsn(k)
        self._update_dcfs(k)
        self._update_ple(k)
        self._update_cmple(k)
        self._update_dtf(k)
        self._update_fm(k)
        self._update_mtf(k)
        self._update_nfc(k)
        self._update_fsafc(k)
        self._update_fcapc(k)
        self._update_fcfpc(k)
        self._update_fce(k)
        self._update_tf(k)
        self._update_cbr(k, jk)
        self._update_b(k, kl)

    def run_population(self):
        """
        Run a sequence of updates to simulate the population sector alone with
        exogenous inputs.

        """
        self.redo_loop = True
        while self.redo_loop:
            self.redo_loop = False
            self.loop0_population(alone=True)
        for k_ in range(1, self.n):
            self.redo_loop = True
            while self.redo_loop:
                self.redo_loop = False
                if self.verbose:
                    print("go loop", k_)
                self.loopk_population(k_-1, k_, k_-1, k_, alone=True)

    @requires(["p1"])
    def _update_state_p1(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        self.p1[k] = self.p1[j] + self.dt*(self.b[jk] - self.d1[jk]
                                           - self.mat1[jk])

    @requires(["p2"])
    def _update_state_p2(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        self.p2[k] = self.p2[j] + self.dt*(self.mat1[jk] - self.d2[jk]
                                           - self.mat2[jk])

    @requires(["p3"])
    def _update_state_p3(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        self.p3[k] = self.p3[j] + self.dt*(self.mat2[jk] - self.d3[jk]
                                           - self.mat3[jk])

    @requires(["p4"])
    def _update_state_p4(self, k, j, jk):
        """
        State variable, requires previous step only
        """
        self.p4[k] = self.p4[j] + self.dt*(self.mat3[jk] - self.d4[jk])

    @requires(["pop"], ["p1", "p2", "p3", "p4"])
    def _update_pop(self, k):
        """
        From step k=0 requires: P1 P2 P3 P4
        """
        self.pop[k] = self.p1[k] + self.p2[k] + self.p3[k] + self.p4[k]

    @requires(["fpu"], ["pop"])
    def _update_fpu(self, k):
        """
        From step k requires: POP
        """
        self.fpu[k] = self.fpu_f(self.pop[k])

    @requires(["lmp"], ["ppolx"])
    def _update_lmp(self, k):
        """
        From step k requires: PPOLX
        """
        self.lmp[k] = self.lmp_f(self.ppolx[k])  # Pollution >

    @requires(["lmf"], ["fpc"])
    def _update_lmf(self, k):
        """
        From step k requires: FPC
        """
        self.lmf[k] = self.lmf_f(self.fpc[k] / self.sfpc)  # Food >

    @requires(["cmi"], ["iopc"])
    def _update_cmi(self, k):
        """
        From step k requires: IOPC
        """
        self.cmi[k] = self.cmi_f(self.iopc[k])  # Industrial Output >

    @requires(["hsapc"], ["sopc"])
    def _update_hsapc(self, k):
        """
        From step k requires: SOPC
        """
        self.hsapc[k] = self.hsapc_f(self.sopc[k])  # Service Output >

    @requires(["ehspc"], ["hsapc"], check_after_init=False)
    def _update_ehspc(self, k):
        """
        From step k=0 requires: HSAPC, else nothing
        """
        self.ehspc[k] = self.smooth_hsapc(k, self.hsid)

    @requires(["lmhs1", "lmhs2", "lmhs"], ["ehspc"])
    def _update_lmhs(self, k):
        """
        From step k requires: EHSPC
        """
        self.lmhs1[k] = self.lmhs1_f(self.ehspc[k])
        self.lmhs2[k] = self.lmhs2_f(self.ehspc[k])
        self.lmhs[k] = clip(self.lmhs2[k], self.lmhs1[k],
                            self.time[k], self.iphst)

    @requires(["lmc"], ["cmi", "fpu"])
    def _update_lmc(self, k):
        """
        From step k requires: CMI FPU
        """
        self.lmc[k] = 1 - self.cmi[k]*self.fpu[k]

    @requires(["m1"], ["le"])
    def _update_m1(self, k):
        """
        From step k requires: LE
        """
        self.m1[k] = self.m1_f(self.le[k])

    @requires(["m2"], ["le"])
    def _update_m2(self, k):
        """
        From step k requires: LE
        """
        self.m2[k] = self.m2_f(self.le[k])

    @requires(["m3"], ["le"])
    def _update_m3(self, k):
        """
        From step k requires: LE
        """
        self.m3[k] = self.m3_f(self.le[k])

    @requires(["m4"], ["le"])
    def _update_m4(self, k):
        """
        From step k requires: LE
        """
        self.m4[k] = self.m4_f(self.le[k])

    @requires(["le"], ["lmf", "lmhs", "lmp", "lmc"])
    def _update_le(self, k):
        """
        From step k requires: LMF LMHS LMP LMC
        """
        self.le[k] = (self.len * self.lmf[k] * self.lmhs[k]
                      * self.lmp[k] * self.lmc[k])

    @requires(["mat1"], ["p1", "m1"])
    def _update_mat1(self, k, kl):
        """
        From step k requires: P1 M1
        """
        self.mat1[kl] = self.p1[k] * (1 - self.m1[k]) / 15

    @requires(["mat2"], ["p2", "m2"])
    def _update_mat2(self, k, kl):
        """
        From step k requires: P2 M2
        """
        self.mat2[kl] = self.p2[k] * (1 - self.m2[k]) / 30

    @requires(["mat3"], ["p3", "m3"])
    def _update_mat3(self, k, kl):
        """
        From step k requires: P3 M3
        """
        self.mat3[kl] = self.p3[k] * (1 - self.m3[k]) / 20

    @requires(["d1"], ["p1", "m1"])
    def _update_d1(self, k, kl):
        """
        From step k requires: P1 M1
        """
        self.d1[kl] = self.p1[k] * self.m1[k]

    @requires(["d2"], ["p2", "m2"])
    def _update_d2(self, k, kl):
        """
        From step k requires: P2 M2
        """
        self.d2[kl] = self.p2[k] * self.m2[k]

    @requires(["d3"], ["p3", "m3"])
    def _update_d3(self, k, kl):
        """
        From step k requires: P3 M3
        """
        self.d3[kl] = self.p3[k] * self.m3[k]

    @requires(["d4"], ["p4", "m4"])
    def _update_d4(self, k, kl):
        """
        From step k requires: P4 M4
        """
        self.d4[kl] = self.p4[k] * self.m4[k]

    @requires(["d"])
    def _update_d(self, k, jk):
        """
        From step k requires: nothing
        """
        self.d[k] = self.d1[jk] + self.d2[jk] + self.d3[jk] + self.d4[jk]

    @requires(["cdr"], ["d", "pop"])
    def _update_cdr(self, k):
        """
        From step k requires: D POP
        """
        self.cdr[k] = 1000 * self.d[k] / self.pop[k]

    @requires(["aiopc"], ["iopc"], check_after_init=False)
    def _update_aiopc(self, k):
        """
        From step k=0 requires: IOPC, else nothing
        """
        self.aiopc[k] = self.smooth_iopc(k, self.ieat)

    @requires(["diopc"], ["iopc"], check_after_init=False)
    def _update_diopc(self, k):
        """
        From step k=0 requires: IOPC, else nothing
        """
        self.diopc[k] = self.dlinf3_iopc(k, self.sad)

    @requires(["fie"], ["iopc", "aiopc"])
    def _update_fie(self, k):
        """
        From step k requires: IOPC AIOPC
        """
        self.fie[k] = (self.iopc[k] - self.aiopc[k]) / self.aiopc[k]

    @requires(["sfsn"], ["diopc"])
    def _update_sfsn(self, k):
        """
        From step k requires: DIOPC
        """
        self.sfsn[k] = self.sfsn_f(self.diopc[k])

    @requires(["frsn"], ["fie"])
    def _update_frsn(self, k):
        """
        From step k requires: FIE
        """
        self.frsn[k] = self.frsn_f(self.fie[k])

    @requires(["dcfs"], ["frsn", "sfsn"])
    def _update_dcfs(self, k):
        """
        From step k requires: FRSN SFSN
        """
        self.dcfs[k] = clip(2.0, self.dcfsn*self.frsn[k]*self.sfsn[k],
                            self.time[k], self.zpgt)

    @requires(["ple"], ["le"], check_after_init=False)
    def _update_ple(self, k):
        """
        From step k=0 requires: LE, else nothing
        """
        self.ple[k] = self.dlinf3_le(k, self.lpd)

    @requires(["cmple"], ["ple"])
    def _update_cmple(self, k):
        """
        From step k requires: PLE
        """
        self.cmple[k] = self.cmple_f(self.ple[k])

    @requires(["dtf"], ["dcfs", "cmple"])
    def _update_dtf(self, k):
        """
        From step k requires: DCFS CMPLE
        """
        self.dtf[k] = self.dcfs[k] * self.cmple[k]

    @requires(["fm"], ["le"])
    def _update_fm(self, k):
        """
        From step k requires: LE
        """
        self.fm[k] = self.fm_f(self.le[k])

    @requires(["mtf"], ["fm"])
    def _update_mtf(self, k):
        """
        From step k requires: FM
        """
        self.mtf[k] = self.mtfn * self.fm[k]

    @requires(["nfc"], ["mtf", "dtf"])
    def _update_nfc(self, k):
        """
        From step k requires: MTF DTF
        """
        self.nfc[k] = self.mtf[k] / self.dtf[k] - 1

    @requires(["fsafc"], ["nfc"])
    def _update_fsafc(self, k):
        """
        From step k requires: NFC
        """
        self.fsafc[k] = self.fsafc_f(self.nfc[k])

    @requires(["fcapc"], ["fsafc", "sopc"])
    def _update_fcapc(self, k):
        """
        From step k requires: FSAFC SOPC
        """
        self.fcapc[k] = self.fsafc[k] * self.sopc[k]  # Service Output >

    @requires(["fcfpc"], ["fcapc"], check_after_init=False)
    def _update_fcfpc(self, k):
        """
        From step k=0 requires: FCAPC, else nothing
        """
        self.fcfpc[k] = self.dlinf3_fcapc(k, self.hsid)

    @requires(["fce"], ["fcfpc"])
    def _update_fce(self, k):
        """
        From step k requires: FCFPC
        """
        self.fce[k] = clip(1.0, self.fce_toclip_f(self.fcfpc[k]),
                           self.time[k], self.fcest)

    @requires(["tf"], ["mtf", "fce", "dtf"])
    def _update_tf(self, k):
        """
        From step k requires: MTF FCE DTF
        """
        self.tf[k] = np.minimum(self.mtf[k], (self.mtf[k]*(1-self.fce[k]) +
                                              self.dtf[k]*self.fce[k]))

    @requires(["cbr"], ["pop"])
    def _update_cbr(self, k, jk):
        """
        From step k requires: POP
        """
        self.cbr[k] = 1000 * self.b[jk] / self.pop[k]

    @requires(["b"], ["d", "p2", "tf"])
    def _update_b(self, k, kl):
        """
        From step k requires: D P2 TF
        """
        self.b[kl] = clip(self.d[k],
                          self.tf[k] * self.p2[k] * 0.5 / self.rlt,
                          self.time[k], self.pet)
