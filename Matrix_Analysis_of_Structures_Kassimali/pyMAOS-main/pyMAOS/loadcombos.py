"""
BSD 3-Clause License
Copyright (c) 2022, Donald N. Bockoven III
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
import itertools


class LoadCombo:

    """
    Class for Load Combinations
    """

    def __init__(
        self, name, factors={}, principle_loads=[], patterned=False, combo_type=None
    ):
        """

        Parameters
        ----------
        name : String
            User defined name for the load combinations.
        factors : TYPE, optional
            Dictionary of Load Factors with Keys set to the load kind string
            and values set to the load factor.
            ie. {"D":1.4,"F":1.4}
        principle_loads : TYPE, optional
            A list of the principle load kinds governing the load combination.
            This list will be used to skip redundant combinations or
            combinations where the member has no loads of the principal
            kind applied.
            The default is [].
        patterned : Boolean, optional
            A boolean to indicate whether the combination should consider load
            patterning. The default is False.
        combo_type : String, optional
            A string to indicate whethe the combination is a design combination
            or a service combination. For beam analysis design combinations will
            limit computations to shear and moment only while service combinations
            will result in full computation of shear, moment, slope and deflection.
            design = "ULS"
            service = "SLS"
            Anything other than "SLS" will be treated as a design designation.
            The default is None.

        Returns
        -------
        None.

        """

        self.name = name
        self.factors = factors
        self.principle_loads = principle_loads
        self.patterned = patterned
        self.combo_type = combo_type

    def __str__(self):
        """
        Determine the output when print() is called on a load combo
        """
        print(f"Combo: {self.name}")
        print(f"Type: {self.combo_type}")
        print(f"Pattern: {self.patterned}")

        key_loads = ""

        for key_load in self.principle_loads:
            key_loads += f"{key_load}"

        print(f"Principle Loads: {key_loads}")

        combo_formula = ""
        i = 0

        for Load_type, factor in self.factors.items():

            if i == 0:
                combo_formula += f"{factor}{Load_type}"
            else:
                combo_formula += f"+{factor}{Load_type}"
            i += 1

        return combo_formula

    def AddLoadCase(self, case_name, factor):
        """
        Adds a load case with its associated load factor
        """

        self.factors[case_name] = factor

    def DeleteLoadCase(self, case_name):
        """
        Deletes a load case with its associated load factor
        """

        del self.factors[case_name]

    def FormulaString(self):
        combo_formula = ""
        i = 0

        for Load_type, factor in self.factors.items():

            if i == 0:
                combo_formula += f"{factor}{Load_type}"
            else:
                if factor < 0:
                    operator = "-"
                else:
                    operator = "+"

                combo_formula += f"{operator}{abs(factor)}{Load_type}"
            i += 1

        return combo_formula


def Full_LoadPatterns(num_spans):
    patterns = []
    n = num_spans
    for r in range(n):
        for item in itertools.combinations(range(n), r):
            check = [1] * n
            for i in item:
                check[i] = 0
            patterns.append(check)
    return patterns


def ACI_LoadPatterns(n, byspan=True):
    pat1 = [1 for i in range(1, n + 1)]  # all spans loaded
    pat2 = [1 if i % 2 == 0 else 0 for i in range(1, n + 1)]  # even spans loaded
    pat3 = [0 if i % 2 == 0 else 1 for i in range(1, n + 1)]  # odd spans loaded

    count = 0
    pat4 = []
    pat5 = []
    pat6 = []
    for i in range(1, n + 1):

        if count <= 1:
            if count == 0:
                pat4.append(1)
                pat5.append(0)
                pat6.append(1)
            else:
                pat4.append(1)
                pat5.append(1)
                pat6.append(0)
            count += 1
        else:
            pat4.append(0)
            pat5.append(1)
            pat6.append(1)
            count = 0

    if n == 1:
        patterns = [pat1]

    elif n == 2:
        patterns = [pat1, pat2, pat3]

    elif n == 3:
        patterns = [pat1, pat2, pat3, pat4, pat5]

    else:
        patterns = [pat1, pat2, pat3, pat4, pat5, pat6]

    if byspan is True:
        patterns_transpose = list(map(list, zip(*patterns)))

        return patterns_transpose
    else:
        return patterns


def IBC2018_Basic(lateral_reversal=True, sls=False):

    if sls:
        service = "SLS"

    else:
        service = None

    combos = [
        LoadCombo("D", {"D": 1}, ["D"], False, service),
        LoadCombo("F", {"F": 1}, ["F"], False, service),
        LoadCombo("L", {"L": 1}, ["L"], True, service),
        LoadCombo("H", {"H": 1}, ["H"], False, service),
        LoadCombo("Lr", {"Lr": 1}, ["Lr"], True, service),
        LoadCombo("S", {"S": 1}, ["S"], True, service),
        LoadCombo("R", {"R": 1}, ["R"], True, service),
        LoadCombo("Wx", {"Wx": 1}, ["Wx"], False, service),
        LoadCombo("Wy", {"Wy": 1}, ["Wy"], False, service),
        LoadCombo("Ex", {"Ex": 1}, ["Ex"], False, service),
        LoadCombo("Ey", {"Ey": 1}, ["Ey"], False, service),
    ]

    if lateral_reversal:

        reversals = [
            LoadCombo("Wxn", {"Wx": -1}, ["Wx"], False, service),
            LoadCombo("Wyn", {"Wy": -1}, ["Wy"], False, service),
            LoadCombo("Exn", {"Ex": -1}, ["Ex"], False, service),
            LoadCombo("Eyn", {"Ey": -1}, ["Ey"], False, service),
        ]

        combos.extend(reversals)

    return combos


def IBC2018_ULS(f1, f2, lateral_reversal=True):

    combos = [
        LoadCombo("16-1", {"D": 1.4, "F": 1.4}, ["D", "F"], False, "ULS"),
        LoadCombo(
            "16-2L", {"D": 1.2, "F": 1.2, "L": 1.6, "H": 1.6}, ["L", "H"], True, "ULS"
        ),
        LoadCombo(
            "16-2Lr",
            {"D": 1.2, "F": 1.2, "L": 1.6, "H": 1.6, "Lr": 0.5},
            ["Lr"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-2S",
            {"D": 1.2, "F": 1.2, "L": 1.6, "H": 1.6, "S": 0.5},
            ["S"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-2R",
            {"D": 1.2, "F": 1.2, "L": 1.6, "H": 1.6, "R": 0.5},
            ["R"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3Lr",
            {"D": 1.2, "F": 1.2, "L": f1, "H": 1.6, "Lr": 1.6},
            ["Lr"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3S",
            {"D": 1.2, "F": 1.2, "L": f1, "H": 1.6, "S": 1.6},
            ["S"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3R",
            {"D": 1.2, "F": 1.2, "L": f1, "H": 1.6, "R": 1.6},
            ["R"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3LrWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 1.6, "Wx": 0.5},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3SWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "S": 1.6, "Wx": 0.5},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3RWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "R": 1.6, "Wx": 0.5},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3LrWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 1.6, "Wy": 0.5},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3SWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "S": 1.6, "Wy": 0.5},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-3RWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "R": 1.6, "Wy": 0.5},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4LrWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 0.5, "Wx": 1, "L": f1},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4SWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "S": 0.5, "Wx": 1, "L": f1},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4RWx",
            {"D": 1.2, "F": 1.2, "H": 1.6, "R": 0.5, "Wx": 1, "L": f1},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4LrWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 0.5, "Wy": 1, "L": f1},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4SWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "S": 0.5, "Wy": 1, "L": f1},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-4RWy",
            {"D": 1.2, "F": 1.2, "H": 1.6, "R": 0.5, "Wy": 1, "L": f1},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-5Ex",
            {"D": 1.2, "F": 1.2, "Ex": 1, "L": f1, "H": 1.6, "S": f2},
            ["Ex"],
            True,
            "ULS",
        ),
        LoadCombo(
            "16-5Ey",
            {"D": 1.2, "F": 1.2, "Ey": 1, "L": f1, "H": 1.6, "S": f2},
            ["Ey"],
            True,
            "ULS",
        ),
        LoadCombo("16-6Wx", {"D": 0.9, "Wx": 1.0, "H": 1.6}, ["Wx"], False, "ULS"),
        LoadCombo("16-6Wy", {"D": 0.9, "Wy": 1.0, "H": 1.6}, ["Wy"], False, "ULS"),
        LoadCombo(
            "16-7Ex", {"D": 0.9, "F": 0.9, "Ex": 1.0, "H": 1.6}, ["Ex"], False, "ULS"
        ),
        LoadCombo(
            "16-7Ey", {"D": 0.9, "F": 0.9, "Ey": 1.0, "H": 1.6}, ["Ey"], False, "ULS"
        ),
    ]

    if lateral_reversal:

        reversals = [
            LoadCombo(
                "16-3LrWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 1.6, "Wx": -0.5},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-3SWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "S": 1.6, "Wx": -0.5},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-3RWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "R": 1.6, "Wx": -0.5},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-3LrWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 1.6, "Wy": -0.5},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-3SWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "S": 1.6, "Wy": -0.5},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-3RWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "R": 1.6, "Wy": -0.5},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4LrWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 0.5, "Wx": -1, "L": f1},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4SWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "S": 0.5, "Wx": -1, "L": f1},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4RWxn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "R": 0.5, "Wx": -1, "L": f1},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4LrWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "Lr": 0.5, "Wy": -1, "L": f1},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4SWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "S": 0.5, "Wy": -1, "L": f1},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-4RWyn",
                {"D": 1.2, "F": 1.2, "H": 1.6, "R": 0.5, "Wy": -1, "L": f1},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-5Exn",
                {"D": 1.2, "F": 1.2, "Ex": -1, "L": f1, "H": 1.6, "S": f2},
                ["Ex"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-5Eyn",
                {"D": 1.2, "F": 1.2, "Ey": -1, "L": f1, "H": 1.6, "S": f2},
                ["Ey"],
                True,
                "ULS",
            ),
            LoadCombo(
                "16-6Wxn", {"D": 0.9, "Wx": -1.0, "H": 1.6}, ["Wx"], False, "ULS"
            ),
            LoadCombo(
                "16-6Wyn", {"D": 0.9, "Wy": -1.0, "H": 1.6}, ["Wy"], False, "ULS"
            ),
            LoadCombo(
                "16-7Exn",
                {"D": 0.9, "F": 0.9, "Ex": -1.0, "H": 1.6},
                ["Ex"],
                False,
                "ULS",
            ),
            LoadCombo(
                "16-7Eyn",
                {"D": 0.9, "F": 0.9, "Ey": -1.0, "H": 1.6},
                ["Ey"],
                False,
                "ULS",
            ),
        ]

        combos.extend(reversals)

    return combos


def IBC2018_ASD(lateral_reversal=True, sls=False):

    if sls:
        service = "SLS"
    else:
        service = "ULS"

    combos = [
        LoadCombo("16-8", {"D": 1, "F": 1}, ["D", "F"], False, service),
        LoadCombo("16-9", {"D": 1, "H": 1, "F": 1, "L": 1}, ["H", "L"], True, service),
        LoadCombo("16-10Lr", {"D": 1, "H": 1, "F": 1, "Lr": 1}, ["Lr"], True, service),
        LoadCombo("16-10S", {"D": 1, "H": 1, "F": 1, "S": 1}, ["S"], True, service),
        LoadCombo("16-10R", {"D": 1, "H": 1, "F": 1, "R": 1}, ["R"], True, service),
        LoadCombo(
            "16-11Lr",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Lr": 0.75},
            ["Lr"],
            True,
            service,
        ),
        LoadCombo(
            "16-11S",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "S": 0.75},
            ["S"],
            True,
            service,
        ),
        LoadCombo(
            "16-11R",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "R": 0.75},
            ["R"],
            True,
            service,
        ),
        LoadCombo(
            "16-12Wx", {"D": 1, "H": 1, "F": 1, "Wx": 0.6}, ["Wx"], False, service
        ),
        LoadCombo(
            "16-12Wy", {"D": 1, "H": 1, "F": 1, "Wy": 0.6}, ["Wy"], False, service
        ),
        LoadCombo(
            "16-12Ex", {"D": 1, "H": 1, "F": 1, "Ex": 0.7}, ["Ex"], False, service
        ),
        LoadCombo(
            "16-12Ey", {"D": 1, "H": 1, "F": 1, "Ey": 0.7}, ["Ey"], False, service
        ),
        LoadCombo(
            "16-13WxLr",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": 0.75 * 0.6, "Lr": 0.75},
            ["Wx"],
            True,
            service,
        ),
        LoadCombo(
            "16-13WxS",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": 0.75 * 0.6, "S": 0.75},
            ["Wx"],
            True,
            service,
        ),
        LoadCombo(
            "16-13WxR",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": 0.75 * 0.6, "R": 0.75},
            ["Wx"],
            True,
            service,
        ),
        LoadCombo(
            "16-13WyLr",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": 0.75 * 0.6, "Lr": 0.75},
            ["Wy"],
            True,
            service,
        ),
        LoadCombo(
            "16-13WyS",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": 0.75 * 0.6, "S": 0.75},
            ["Wy"],
            True,
            service,
        ),
        LoadCombo(
            "16-13WyR",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": 0.75 * 0.6, "R": 0.75},
            ["Wy"],
            True,
            service,
        ),
        LoadCombo(
            "16-14Ex",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "S": 0.75, "Ex": 0.75 * 0.7},
            ["Ex"],
            True,
            service,
        ),
        LoadCombo(
            "16-14Ey",
            {"D": 1, "H": 1, "F": 1, "L": 0.75, "S": 0.75, "Ey": 0.75 * 0.7},
            ["Ey"],
            True,
            service,
        ),
        LoadCombo("16-15Wx", {"D": 0.6, "H": 1, "Wx": 0.6}, ["Wx"], False, service),
        LoadCombo("16-15Wy", {"D": 0.6, "H": 1, "Wy": 0.6}, ["Wy"], False, service),
        LoadCombo(
            "16-16Ex", {"D": 0.6, "F": 0.6, "H": 1, "Ex": 0.7}, ["Ex"], False, service
        ),
        LoadCombo(
            "16-16Ey", {"D": 0.6, "F": 0.6, "H": 1, "Ey": 0.7}, ["Ey"], False, service
        ),
    ]

    if lateral_reversal:

        reversals = [
            LoadCombo(
                "16-12Wxn", {"D": 1, "H": 1, "F": 1, "Wx": -0.6}, ["Wx"], False, service
            ),
            LoadCombo(
                "16-12Wyn", {"D": 1, "H": 1, "F": 1, "Wy": -0.6}, ["Wy"], False, service
            ),
            LoadCombo(
                "16-12Exn", {"D": 1, "H": 1, "F": 1, "Ex": -0.7}, ["Ex"], False, service
            ),
            LoadCombo(
                "16-12Eyn", {"D": 1, "H": 1, "F": 1, "Ey": -0.7}, ["Ey"], False, service
            ),
            LoadCombo(
                "16-13WxnLr",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": -0.75 * 0.6, "Lr": 0.75},
                ["Wx"],
                True,
                service,
            ),
            LoadCombo(
                "16-13WxnS",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": -0.75 * 0.6, "S": 0.75},
                ["Wx"],
                True,
                service,
            ),
            LoadCombo(
                "16-13WxnR",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wx": -0.75 * 0.6, "R": 0.75},
                ["Wx"],
                True,
                service,
            ),
            LoadCombo(
                "16-13WynLr",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": -0.75 * 0.6, "Lr": 0.75},
                ["Wy"],
                True,
                service,
            ),
            LoadCombo(
                "16-13WynS",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": -0.75 * 0.6, "S": 0.75},
                ["Wy"],
                True,
                service,
            ),
            LoadCombo(
                "16-13WynR",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "Wy": -0.75 * 0.6, "R": 0.75},
                ["Wy"],
                True,
                service,
            ),
            LoadCombo(
                "16-14Exn",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "S": 0.75, "Ex": -0.75 * 0.7},
                ["Ex"],
                True,
                service,
            ),
            LoadCombo(
                "16-14Eyn",
                {"D": 1, "H": 1, "F": 1, "L": 0.75, "S": 0.75, "Ey": -0.75 * 0.7},
                ["Ey"],
                True,
                service,
            ),
            LoadCombo(
                "16-15Wxn", {"D": 0.6, "H": 1, "Wx": -0.6}, ["Wx"], False, service
            ),
            LoadCombo(
                "16-15Wyn", {"D": 0.6, "H": 1, "Wy": -0.6}, ["Wy"], False, service
            ),
            LoadCombo(
                "16-16Exn",
                {"D": 0.6, "F": 0.6, "H": 1, "Ex": -0.7},
                ["Ex"],
                False,
                service,
            ),
            LoadCombo(
                "16-16Eyn",
                {"D": 0.6, "F": 0.6, "H": 1, "Ey": -0.7},
                ["Ey"],
                False,
                service,
            ),
        ]

        combos.extend(reversals)

    return combos


def BCBC2018_T4132A(lateral_reversal=True, gravityoverturn=False):

    combos = [
        LoadCombo("T4132-A_C1", {"D": 1.4, "H": 1.5}, ["D", "H"], False, "ULS"),
        LoadCombo(
            "T4132-A_C2_D",
            {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25},
            ["L", "Lr", "F"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C2_DS",
            {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "S": 1.0},
            ["S"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C2_DWx",
            {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wx": 0.4},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C2_9DWx",
            {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wx": 0.4},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C2_DWy",
            {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wy": 0.4},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C2_9DWy",
            {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wy": 0.4},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo("T4132-A_C3_D", {"D": 1.25, "H": 1.5, "S": 1.5}, ["S"], True, "ULS"),
        LoadCombo(
            "T4132-A_C3_DL",
            {"D": 1.25, "H": 1.5, "L": 1.0, "Lr": 1.5, "F": 1.0, "S": 1.5},
            ["L", "F", "Lr", "S"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C3_DWx",
            {"D": 1.25, "H": 1.5, "S": 1.5, "Wx": 0.4},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C3_9DWx",
            {"D": 0.9, "H": 1.5, "S": 1.5, "Wx": 0.4},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C3_DWy",
            {"D": 1.25, "H": 1.5, "S": 1.5, "Wy": 0.4},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C3_9DWy",
            {"D": 0.9, "H": 1.5, "S": 1.5, "Wy": 0.4},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_DWx", {"D": 1.25, "H": 1.5, "Wx": 1.4}, ["Wx"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C4_9DWx", {"D": 0.9, "H": 1.5, "Wx": 1.4}, ["Wx"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C4_DWy", {"D": 1.25, "H": 1.5, "Wy": 1.4}, ["Wy"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C4_9DWy", {"D": 0.9, "H": 1.5, "Wy": 1.4}, ["Wy"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C4_DWxL",
            {"D": 1.25, "H": 1.5, "Wx": 1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_9DWxL",
            {"D": 0.9, "H": 1.5, "Wx": 1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_DWyL",
            {"D": 1.25, "H": 1.5, "Wy": 1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_9DWyL",
            {"D": 0.9, "H": 1.5, "Wy": 1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_DWxS",
            {"D": 1.25, "H": 1.5, "Wx": 1.4, "S": 0.5},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_9DWxS",
            {"D": 0.9, "H": 1.5, "Wx": 1.4, "S": 0.5},
            ["Wx"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_DWyS",
            {"D": 1.25, "H": 1.5, "Wy": 1.4, "S": 0.5},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C4_9DWyS",
            {"D": 0.9, "H": 1.5, "Wy": 1.4, "S": 0.5},
            ["Wy"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C5_DEx", {"D": 1.0, "H": 1.5, "Ex": 1.0}, ["Ex"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C5_DEy", {"D": 1.0, "H": 1.5, "Ey": 1.0}, ["Ey"], False, "ULS"
        ),
        LoadCombo(
            "T4132-A_C5_DExLS",
            {"D": 1.0, "H": 1.5, "Ex": 1.0, "L": 0.5, "F": 0.5, "Lr": 1.0, "S": 0.25},
            ["Ex"],
            True,
            "ULS",
        ),
        LoadCombo(
            "T4132-A_C5_DEyLS",
            {"D": 1.0, "H": 1.5, "Ey": 1.0, "L": 0.5, "F": 0.5, "Lr": 1.0, "S": 0.25},
            ["Ey"],
            True,
            "ULS",
        ),
    ]

    if gravityoverturn:

        overturn = [
            LoadCombo(
                "T4132-A_C2_9D",
                {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25},
                ["L", "Lr", "F"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C2_9DS",
                {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "S": 1.0},
                ["S"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C3_9D", {"D": 0.9, "H": 1.5, "S": 1.5}, ["S"], True, "ULS"
            ),
            LoadCombo(
                "T4132-A_C3_9DL",
                {"D": 0.9, "H": 1.5, "L": 1.0, "Lr": 1.5, "F": 1.0, "S": 1.5},
                ["L", "F", "Lr"],
                True,
                "ULS",
            ),
        ]

        combos.extend(overturn)

    if lateral_reversal:

        reversals = [
            LoadCombo(
                "T4132-A_C2_DWxn",
                {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wx": -0.4},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C2_9DWxn",
                {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wx": -0.4},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C2_DWyn",
                {"D": 1.25, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wy": -0.4},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C2_9DWyn",
                {"D": 0.9, "H": 1.5, "L": 1.5, "Lr": 1.5, "F": 1.25, "Wy": -0.4},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C3_DWxn",
                {"D": 1.25, "H": 1.5, "S": 1.5, "Wx": -0.4},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C3_9DWxn",
                {"D": 0.9, "H": 1.5, "S": 1.5, "Wx": -0.4},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C3_DWyn",
                {"D": 1.25, "H": 1.5, "S": 1.5, "Wy": -0.4},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C3_9DWyn",
                {"D": 0.9, "H": 1.5, "S": 1.5, "Wy": -0.4},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWxn",
                {"D": 1.25, "H": 1.5, "Wx": -1.4},
                ["Wx"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWxn",
                {"D": 0.9, "H": 1.5, "Wx": -1.4},
                ["Wx"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWyn",
                {"D": 1.25, "H": 1.5, "Wy": -1.4},
                ["Wy"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWyn",
                {"D": 0.9, "H": 1.5, "Wy": -1.4},
                ["Wy"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWxnL",
                {"D": 1.25, "H": 1.5, "Wx": -1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWxnL",
                {"D": 0.9, "H": 1.5, "Wx": -1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWynL",
                {"D": 1.25, "H": 1.5, "Wy": -1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWynL",
                {"D": 0.9, "H": 1.5, "Wy": -1.4, "L": 0.5, "F": 0.5, "Lr": 1.0},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWxnS",
                {"D": 1.25, "H": 1.5, "Wx": -1.4, "S": 0.5},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWxnS",
                {"D": 0.9, "H": 1.5, "Wx": -1.4, "S": 0.5},
                ["Wx"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_DWynS",
                {"D": 1.25, "H": 1.5, "Wy": -1.4, "S": 0.5},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C4_9DWynS",
                {"D": 0.9, "H": 1.5, "Wy": -1.4, "S": 0.5},
                ["Wy"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C5_DExn",
                {"D": 1.0, "H": 1.5, "Ex": -1.0},
                ["Ex"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C5_DEyn",
                {"D": 1.0, "H": 1.5, "Ey": -1.0},
                ["Ey"],
                False,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C5_DExnLS",
                {
                    "D": 1.0,
                    "H": 1.5,
                    "Ex": -1.0,
                    "L": 0.5,
                    "F": 0.5,
                    "Lr": 1.0,
                    "S": 0.25,
                },
                ["Ex"],
                True,
                "ULS",
            ),
            LoadCombo(
                "T4132-A_C5_DEynLS",
                {
                    "D": 1.0,
                    "H": 1.5,
                    "Ey": -1.0,
                    "L": 0.5,
                    "F": 0.5,
                    "Lr": 1.0,
                    "S": 0.25,
                },
                ["Ey"],
                True,
                "ULS",
            ),
        ]

        combos.extend(reversals)

    return combos
