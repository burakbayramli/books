import math
import numpy as np


def polynomial_evaluation(c_list, x):
    """
    evaluate a polynomial defined by a list of coeff. in ascending order
    C0 + C1x + C2x^2 + ... + Cnx^n = [C0,C1,C2,...,Cn]
    """
    i = 0
    res = 0
    if all(c == 0 for c in c_list):
        pass
    else:
        for c in c_list:
            res = res + c * math.pow(x, i)
            i += 1
    return res


class Piecewise_Polynomial:
    def __init__(self, functions=[[[0], [0, 0]]]):

        self.functions = functions

    def __str__(self):

        out = ""

        for line in self.functions:

            func = ""

            for i, coeff in enumerate(line[0]):

                if i == 0:
                    func += f"{coeff}"
                else:

                    func += f" + {coeff} x^{i}"
            func += f"  for {line[1][0]} <= x <= {line[1][1]}\n"

            out += func
        return out

    def evaluate(self, x):
        """
        Given a piecewise function and an x evaluate the results
        """
        # in the context of the beam model a tolerance of 1E-6 will
        # yield acceptable results as we are evaluating normal polynomials
        tol = 0.000001

        # initialize res to avoid an ref before assignment error in
        # the case where the below reaches pass for all conditions.

        piece_function = self.functions

        res = 0

        if piece_function == []:
            res = 0
        else:
            for line in piece_function:
                if (line[1][0] - tol) < x <= (line[1][1] + tol):
                    res = polynomial_evaluation(line[0], x)
                else:
                    # x is not in the current functions range
                    pass
        return res

    def roots(self):
        """
        Given a piecewise function return a list
        of the location of zeros or sign change
        """

        piece_function = self.functions

        zero_loc = []
        i = 0
        for line in piece_function:

            if len(line[0]) == 1 and i == 0:
                pass  # If function is a value then there is no chance for a sign change
            else:
                a = polynomial_evaluation(
                    line[0], line[1][0] + 0.0001
                )  # value at start of bounds
                b = polynomial_evaluation(
                    line[0], line[1][1] - 0.0001
                )  # value at end of bounds

                if a == 0:
                    zero_loc.append(line[1][0])
                elif b == 0:
                    zero_loc.append(line[1][1])
                else:
                    # if signs are the the same a/b will result in a positive value
                    coeff = line[0][::-1]
                    c = np.roots(coeff)
                    # Some real solutions may contain a very small imaginary part
                    # account for this with a tolerance on the imaginary
                    # part of 1e-5
                    c = c.real[abs(c.imag) < 1e-5]
                    for root in c:
                        # We only want roots that are with the piece range
                        if line[1][0] < root <= line[1][1]:
                            zero_loc.append(root)
                        else:
                            pass
                if i == 0:
                    pass
                else:
                    # value at end of previous bounds
                    d = polynomial_evaluation(
                        piece_function[i - 1][0], line[1][0] - 0.0001
                    )

                    if d == 0:
                        pass
                    elif a / d < 0:
                        zero_loc.append(line[1][0])
                    else:
                        pass
            i += 1
        zero_loc = sorted(set(zero_loc))

        return zero_loc

    def combine(self, other, LF, LFother):
        """
        Join two piecewise functions to create one piecewise function ecompassing
        the ranges and polynomials associated with each
        """
        Fa = self.functions
        Fb = other.functions
        LFa = LF
        LFb = LFother

        functions = [Fa, Fb]
        LF = [LFa, LFb]

        # Gather the ranges for each piece of the the two input functions
        ab = []
        for func in Fa:
            ab.append(func[1][0])
            ab.append(func[1][1])
        for func in Fb:
            ab.append(func[1][0])
            ab.append(func[1][1])
        ab = list(set(ab))
        ab.sort()

        f_out = []

        for i, j in enumerate(ab):
            if i == 0:
                piece_range = [0, j]
            else:
                piece_range = [ab[i - 1], j]
            if piece_range == [0, 0]:
                pass
            else:
                f = []

                for i, func in enumerate(functions):

                    for piece in func:

                        if (
                            piece[1][0] < piece_range[1]
                            and piece[1][1] >= piece_range[1]
                        ):
                            # difference in number of coefficients
                            eq_len_delta = len(piece[0]) - len(f)

                            if eq_len_delta > 0:
                                f.extend([0] * eq_len_delta)
                            elif eq_len_delta < 0:
                                piece[0].extend([0] * abs(eq_len_delta))
                            else:
                                pass
                            f = [j * LF[i] + k for j, k in zip(piece[0], f)]
                        else:
                            pass
                f_out.append([f, piece_range])
        return Piecewise_Polynomial(f_out)


class R2_Point_Moment:
    def __init__(self, M, a, member, loadcase="D"):
        """

        Parameters
        ----------
        M : FLOAT
            Applied moment, counter-clockwise positive.
        a : FLOAT
            Point of application of moment as measured from the member left end.
        member : Element Class
            the member that the load is applied to.
        loadcase : STRING, optional
            String representation of the applied load type, this
            data is used for load cases and combindations. The default is "D".

        Returns
        -------
        None.

        """
        self.M = M
        self.a = a
        self.L = member.length

        self.E = member.material.E
        self.I = member.section.Ixx

        self.EI = self.E * self.I

        self.kind = "MOMENT"
        self.loadcase = loadcase

        # Constants of Integration
        self.integration_constants()

        # Simple End Reactions
        self.Riy = self.M / self.L
        self.Rjy = -1 * self.Riy

        # Piecewise Functions
        # [co....cn x^n] [xa, xb]

        Vy = [[[self.Riy], [0, self.a]], [[self.Riy], [self.a, self.L]]]

        Mz = [
            [[0, self.Riy], [0, self.a]],
            [[-1 * self.M, self.Riy], [self.a, self.L]],
        ]

        Sz = [
            [[self.c1 / self.EI, 0, self.Riy / (2 * self.EI)], [0, self.a]],
            [
                [
                    self.c2 / self.EI,
                    -1 * self.M / self.EI,
                    self.Riy / (2 * self.EI),
                ],
                [self.a, self.L],
            ],
        ]

        Dy = [
            [
                [
                    self.c3 / self.EI,
                    self.c1 / self.EI,
                    0,
                    self.Riy / (6 * self.EI),
                ],
                [0, self.a],
            ],
            [
                [
                    self.c4 / self.EI,
                    self.c2 / self.EI,
                    -1 * self.M / (2 * self.EI),
                    self.Riy / (6 * self.EI),
                ],
                [self.a, self.L],
            ],
        ]

        self.Wx = Piecewise_Polynomial()  # Axial Load Function
        self.Wy = Piecewise_Polynomial()  # Vertical Load Function
        self.Ax = Piecewise_Polynomial()
        self.Dx = Piecewise_Polynomial()
        self.Vy = Piecewise_Polynomial(Vy)
        self.Mz = Piecewise_Polynomial(Mz)
        self.Sz = Piecewise_Polynomial(Sz)
        self.Dy = Piecewise_Polynomial(Dy)

    def integration_constants(self):
        M = self.M
        a = self.a
        L = self.L

        # Constants of Integration
        self.c1 = ((3 * M * a * a) - (6 * L * M * a) + (2 * L * L * M)) / (6 * L)

        self.c2 = ((3 * M * a * a) + (2 * L * L * M)) / (6 * L)

        self.c3 = 0

        self.c4 = -1 / 2 * M * a * a

    def FEF(self):
        """
        Compute and return the fixed and forces

        Returns
        -------
        None.

        """
        M = self.M
        a = self.a
        L = self.L

        Miz = -1 * (M * (a - L) * ((3 * a) - L)) / (L * L)
        Mjz = -1 * (M * a * (3 * a - 2 * L)) / (L * L)
        Riy = self.Riy + (Miz / L) + (Mjz / L)
        Rjy = self.Rjy - (Miz / L) - (Mjz / L)

        return [0, Riy, Miz, 0, Rjy, Mjz]


class R2_Point_Load:
    def __init__(self, p, a, member, loadcase="D"):

        self.p = p
        self.a = a
        self.L = member.length

        self.E = member.material.E
        self.I = member.section.Ixx

        self.EI = self.E * self.I

        self.kind = "POINT"
        self.loadcase = loadcase

        # Constants of Integration
        self.integration_constants()

        # Simple End Reactions
        self.Riy = self.p * ((self.a - self.L) / self.L)
        self.Rjy = -1 * self.p * self.a * (1 / self.L)

        # Piecewise Functions
        # [co....cn x^n] [xa, xb]
        Vy = [
            [[self.Riy], [0, self.a]],
            [[self.Riy + self.p], [self.a, self.L]],
        ]

        Mz = [
            [[self.c1, self.Riy], [0, self.a]],
            [[self.c2, self.Riy + self.p], [self.a, self.L]],
        ]

        Sz = [
            [[self.c3, self.c1, self.Riy / 2], [0, self.a]],
            [[self.c4, self.c2, (self.Riy + self.p) / 2], [self.a, self.L]],
        ]

        Dy = [
            [[self.c5, self.c3, self.c1 / 2, self.Riy / 6], [0, self.a]],
            [
                [self.c6, self.c4, self.c2 / 2, (self.Riy + self.p) / 6],
                [self.a, self.L],
            ],
        ]

        Sz[0][0] = [i / self.EI for i in Sz[0][0]]
        Sz[1][0] = [i / self.EI for i in Sz[1][0]]

        Dy[0][0] = [i / self.EI for i in Dy[0][0]]
        Dy[1][0] = [i / self.EI for i in Dy[1][0]]

        self.Wx = Piecewise_Polynomial()  # Axial Load Function
        self.Wy = Piecewise_Polynomial()  # Vertical Load Function
        self.Ax = Piecewise_Polynomial()
        self.Dx = Piecewise_Polynomial()
        self.Vy = Piecewise_Polynomial(Vy)
        self.Mz = Piecewise_Polynomial(Mz)
        self.Sz = Piecewise_Polynomial(Sz)
        self.Dy = Piecewise_Polynomial(Dy)

    def integration_constants(self):

        P = self.p
        a = self.a
        L = self.L

        self.c1 = 0
        self.c2 = -1 * P * a
        self.c3 = (P * a * (a - (2 * L)) * (a - L)) / (6 * L)
        self.c4 = (P * a * ((a * a) + (2 * L * L))) / (6 * L)
        self.c5 = 0
        self.c6 = (-1 * P * a * a * a) / 6

    def FEF(self):

        P = self.p
        a = self.a
        L = self.L

        Miz = -1 * (P * a * (a - L) * (a - L)) / (L * L)
        Mjz = -1 * (P * a * a * (a - L)) / (L * L)
        Riy = self.Riy + (Miz / L) + (Mjz / L)
        Rjy = self.Rjy - (Miz / L) - (Mjz / L)

        return [0, Riy, Miz, 0, Rjy, Mjz]


class R2_Linear_Load:
    def __init__(self, w1, w2, a, b, member, loadcase="D"):

        self.w1 = w1
        self.w2 = w2
        self.a = a
        self.b = b
        self.c = b - a
        self.L = member.length

        self.E = member.material.E
        self.I = member.section.Ixx

        self.EI = self.E * self.I

        self.kind = "LINE"
        self.loadcase = loadcase

        # Constants of Integration
        self.integration_constants()

        # Simple End Reactions
        self.W = 0.5 * self.c * (self.w2 + self.w1)
        self.cbar = ((self.w1 + (2 * self.w2)) / (3 * (self.w2 + self.w1))) * self.c

        self.Rjy = -1 * self.W * (self.a + self.cbar) * (1 / self.L)
        self.Riy = -1 * self.W - self.Rjy

        # Piecewise Functions
        # [co....cn x^n] [xa, xb]
        Wy = [
            [[0], [0, self.a]],
            [
                [
                    ((-1 * self.a * self.w2) - (self.c * self.w1) - (self.a * self.w1))
                    / self.c,
                    (self.w2 - self.w1) / self.c,
                ],
                [self.a, self.b],
            ],
            [[0], [self.b, self.L]],
        ]

        Vy = [
            [[self.c1], [0, self.a]],
            [
                [
                    self.c2,
                    self.w1
                    + ((self.a * self.w1) / self.c)
                    - ((self.a * self.w2) / self.c),
                    (self.w2 / (2 * self.c)) - (self.w1 / (2 * self.c)),
                ],
                [self.a, self.b],
            ],
            [[self.c3], [self.b, self.L]],
        ]

        Mz = [
            [[self.c4, self.c1], [0, self.a]],
            [
                [
                    self.c5,
                    self.c2,
                    (self.w1 / 2)
                    + ((self.a * self.w1) / (2 * self.c))
                    - ((self.a * self.w2) / (2 * self.c)),
                    (self.w2 / (6 * self.c)) - (self.w1 / (6 * self.c)),
                ],
                [self.a, self.b],
            ],
            [[self.c6, self.c3], [self.b, self.L]],
        ]

        Sz = [
            [[self.c7, self.c4, 0.5 * self.c1], [0, self.a]],
            [
                [
                    self.c8,
                    self.c5,
                    0.5 * self.c2,
                    (self.w1 / 6)
                    + ((self.a * self.w1) / (6 * self.c))
                    - ((self.a * self.w2) / (6 * self.c)),
                    (self.w2 / (24 * self.c)) - (self.w1 / (24 * self.c)),
                ],
                [self.a, self.b],
            ],
            [[self.c9, self.c6, 0.5 * self.c3], [self.b, self.L]],
        ]

        Dy = [
            [[self.c10, self.c7, 0.5 * self.c4, self.c1 / 6], [0, self.a]],
            [
                [
                    self.c11,
                    self.c8,
                    0.5 * self.c5,
                    self.c2 / 6,
                    (self.w1 / 24)
                    + ((self.a * self.w1) / (24 * self.c))
                    - ((self.a * self.w2) / (24 * self.c)),
                    (self.w2 / (120 * self.c)) - (self.w1 / (120 * self.c)),
                ],
                [self.a, self.b],
            ],
            [
                [self.c12, self.c9, 0.5 * self.c6, self.c3 / 6],
                [self.b, self.L],
            ],
        ]

        Sz[0][0] = [i / self.EI for i in Sz[0][0]]
        Sz[1][0] = [i / self.EI for i in Sz[1][0]]
        Sz[2][0] = [i / self.EI for i in Sz[2][0]]

        Dy[0][0] = [i / self.EI for i in Dy[0][0]]
        Dy[1][0] = [i / self.EI for i in Dy[1][0]]
        Dy[2][0] = [i / self.EI for i in Dy[2][0]]

        self.Wx = Piecewise_Polynomial()  # Axial Load Function
        self.Wy = Piecewise_Polynomial(Wy)  # Vertical Load Function
        self.Ax = Piecewise_Polynomial()
        self.Dx = Piecewise_Polynomial()
        self.Vy = Piecewise_Polynomial(Vy)
        self.Mz = Piecewise_Polynomial(Mz)
        self.Sz = Piecewise_Polynomial(Sz)
        self.Dy = Piecewise_Polynomial(Dy)

    def integration_constants(self):

        w1 = self.w1
        w2 = self.w2
        a = self.a
        b = self.b
        L = self.L

        self.c1 = (
            (((2 * b * b) + ((-a - 3 * L) * b) - (a * a) + (3 * L * a)) * w2)
            + (((b * b) + ((a - 3 * L) * b) - (2 * a * a) + (3 * L * a)) * w1)
        ) / (6 * L)
        self.c2 = (
            (
                (
                    (2 * b * b * b)
                    + ((-3 * a - 3 * L) * b * b)
                    + (6 * L * a * b)
                    + (a * a * a)
                )
                * w2
            )
            + (((b * b * b) - (3 * L * b * b) - (3 * a * a * b) + (2 * a * a * a)) * w1)
        ) / (6 * L * b - 6 * L * a)
        self.c3 = (
            ((2 * b * b - a * b - a * a) * w2) + ((b * b + a * b - 2 * a * a) * w1)
        ) / (6 * L)
        self.c4 = 0
        self.c5 = (
            -1
            * ((a * a * a * w2) + ((2 * a * a * a - 3 * a * a * b) * w1))
            / (6 * b - 6 * a)
        )
        self.c6 = (
            -1
            * ((2 * b * b - a * b - a * a) * w2 + (b * b + a * b - 2 * a * a) * w1)
            / 6
        )
        self.c7 = (
            (
                12 * b * b * b * b
                + (-3 * a - 45 * L) * b * b * b
                + (-3 * a * a + 15 * L * a + 40 * L * L) * b * b
                + (-3 * a * a * a + 15 * L * a * a - 20 * L * L * a) * b
                - 3 * a * a * a * a
                + 15 * L * a * a * a
                - 20 * L * L * a * a
            )
            * w2
            + (
                3 * b * b * b * b
                + (3 * a - 15 * L) * b * b * b
                + (3 * a * a - 15 * L * a + 20 * L * L) * b * b
                + (3 * a * a * a - 15 * L * a * a + 20 * L * L * a) * b
                - 12 * a * a * a * a
                + 45 * L * a * a * a
                - 40 * L * L * a * a
            )
            * w1
        ) / (360 * L)
        self.c8 = (
            (
                12 * b * b * b * b * b
                + (-15 * a - 45 * L) * b * b * b * b
                + (60 * L * a + 40 * L * L) * b * b * b
                - 60 * L * L * a * b * b
                + 3 * a * a * a * a * a
                + 20 * L * L * a * a * a
            )
            * w2
            + (
                3 * b * b * b * b * b
                - 15 * L * b * b * b * b
                + 20 * L * L * b * b * b
                + (-15 * a * a * a * a - 60 * L * L * a * a) * b
                + 12 * a * a * a * a * a
                + 40 * L * L * a * a * a
            )
            * w1
        ) / (360 * L * b - 360 * L * a)
        self.c9 = (
            (
                12 * b * b * b * b
                - 3 * a * b * b * b
                + (40 * L * L - 3 * a * a) * b * b
                + (-3 * a * a * a - 20 * L * L * a) * b
                - 3 * a * a * a * a
                - 20 * L * L * a * a
            )
            * w2
            + (
                3 * b * b * b * b
                + 3 * a * b * b * b
                + (3 * a * a + 20 * L * L) * b * b
                + (3 * a * a * a + 20 * L * L * a) * b
                - 12 * a * a * a * a
                - 40 * L * L * a * a
            )
            * w1
        ) / (360 * L)
        self.c10 = 0
        self.c11 = (
            -1
            * (
                a * a * a * a * a * w2
                + (4 * a * a * a * a * a - 5 * a * a * a * a * b) * w1
            )
            / (120 * b - 120 * a)
        )
        self.c12 = (
            -1
            * (
                (
                    4 * b * b * b * b
                    - a * b * b * b
                    - a * a * b * b
                    - a * a * a * b
                    - a * a * a * a
                )
                * w2
                + (
                    b * b * b * b
                    + a * b * b * b
                    + a * a * b * b
                    + a * a * a * b
                    - 4 * a * a * a * a
                )
                * w1
            )
            / 120
        )

    def FEF(self):

        L = self.L
        c3 = self.c3
        c6 = self.c6
        c7 = self.c7
        c9 = self.c9

        Miz = -1 * (c3 * L * L + 2 * c6 * L + 2 * c9 + 4 * c7) / L
        Mjz = -1 * (2 * c3 * L * L + 4 * c6 * L + 4 * c9 + 2 * c7) / L
        Riy = self.Riy + (Miz / L) + (Mjz / L)
        Rjy = self.Rjy - (Miz / L) - (Mjz / L)

        return [0, Riy, Miz, 0, Rjy, Mjz]


class R2_Axial_Load:
    def __init__(self, p, a, member, loadcase="D"):

        self.p = p
        self.a = a
        self.L = member.length

        self.E = member.material.E
        self.A = member.section.Area

        self.EA = self.E * self.A

        self.kind = "AXIAL_POINT"
        self.loadcase = loadcase

        # Simple End Reactions

        self.Rix = -1 * self.p
        self.Rjx = 0

        # Constants of Integration
        self.integration_constants()

        # Piecewise Functions
        # [co....cn x^n] [xa, xb]
        Ax = [
            [[-1 * self.Rix], [0, self.a]],
            [[-1 * self.Rix - self.p], [self.a, self.L]],
        ]

        Dx = [
            [[self.c1, -1 * self.Rix], [0, self.a]],
            [[self.c2, -1 * self.Rix - self.p], [self.a, self.L]],
        ]

        Dx[0][0] = [i / self.EA for i in Dx[0][0]]
        Dx[1][0] = [i / self.EA for i in Dx[1][0]]

        self.Wx = Piecewise_Polynomial()  # Axial Load Function
        self.Wy = Piecewise_Polynomial()  # Vertical Load Function
        self.Ax = Piecewise_Polynomial(Ax)
        self.Dx = Piecewise_Polynomial(Dx)
        self.Vy = Piecewise_Polynomial()
        self.Mz = Piecewise_Polynomial()
        self.Sz = Piecewise_Polynomial()
        self.Dy = Piecewise_Polynomial()

    def integration_constants(self):

        p = self.p
        a = self.a

        self.c1 = 0
        self.c2 = p * a

    def FEF(self):

        p = self.p
        a = self.a
        L = self.L

        Rix = (p * (a - L)) / L
        Rjx = (-1 * p * a) / L

        return [Rix, 0, 0, Rjx, 0, 0]


class R2_Axial_Linear_Load:
    def __init__(self, w1, w2, a, b, member, loadcase="D"):

        self.w1 = w1
        self.w2 = w2
        self.a = a
        self.b = b
        self.c = b - a
        self.L = member.length

        self.E = member.material.E
        self.A = member.section.Area

        self.EA = self.E * self.A

        self.kind = "AXIAL_LINE"
        self.loadcase = loadcase

        # Simple End Reactions
        self.W = 0.5 * self.c * (self.w2 + self.w1)

        self.Rix = -1 * self.W
        self.Rjx = 0

        # Constants of Integration
        self.integration_constants()

        # Piecewise Functions
        # [co....cn x^n] [xa, xb]
        Wx = [
            [[0], [0, self.a]],
            [
                [
                    ((-1 * self.a * self.w2) - (self.c * self.w1) - (self.a * self.w1))
                    / self.c,
                    (self.w2 - self.w1) / self.c,
                ],
                [self.a, self.b],
            ],
            [[0], [self.b, self.L]],
        ]

        Ax = [
            [[-1 * self.Rix], [0, self.a]],
            [
                [
                    self.c1,
                    (self.a * self.w2 - self.b * self.w1) / (self.c),
                    -1 * (self.w2 - self.w1) / (2 * self.c),
                ],
                [self.a, self.b],
            ],
            [[-1 * self.Rix - self.W], [self.b, self.L]],
        ]

        Dx = [
            [[self.c2, -1 * self.Rix], [0, self.a]],
            [
                [
                    self.c3,
                    self.c1,
                    ((self.a * self.w2 - self.b * self.w1)) / (2 * self.c),
                    -1 * ((self.w2 - self.w1)) / (6 * self.c),
                ],
                [self.a, self.b],
            ],
            [[self.c4, -1 * self.Rix - self.W], [self.b, self.L]],
        ]

        Dx[0][0] = [i / self.EA for i in Dx[0][0]]
        Dx[1][0] = [i / self.EA for i in Dx[1][0]]
        Dx[2][0] = [i / self.EA for i in Dx[2][0]]

        self.Wx = Piecewise_Polynomial(Wx)  # Axial Load Function
        self.Wy = Piecewise_Polynomial()  # Vertical Load Function
        self.Ax = Piecewise_Polynomial(Ax)
        self.Dx = Piecewise_Polynomial(Dx)
        self.Vy = Piecewise_Polynomial()
        self.Mz = Piecewise_Polynomial()
        self.Sz = Piecewise_Polynomial()
        self.Dy = Piecewise_Polynomial()

    def integration_constants(self):

        w1 = self.w1
        w2 = self.w2
        a = self.a
        b = self.b
        Ri = self.Rix

        self.c1 = -(
            (a * a * w2 - 2 * a * b * w1 + a * a * w1 + 2 * Ri * b - 2 * Ri * a)
            / (2 * (b - a))
        )

        self.c2 = 0

        self.c3 = (a * a * (a * w2 - 3 * b * w1 + 2 * a * w1)) / (6 * (b - a))

        self.c4 = (
            (2 * b * b - a * b - a * a) * w2 + (b * b + a * b - 2 * a * a) * w1
        ) / 6

    def FEF(self):

        w1 = self.w1
        w2 = self.w2
        a = self.a
        b = self.b
        L = self.L

        Rix = (
            (b - a)
            * (2 * b * w2 + a * w2 - 3 * L * w2 + b * w1 + 2 * a * w1 - 3 * L * w1)
        ) / (6 * L)
        Rjx = -1 * (((b - a) * (2 * b * w2 + a * w2 + b * w1 + 2 * a * w1)) / (6 * L))

        return [Rix, 0, 0, Rjx, 0, 0]
