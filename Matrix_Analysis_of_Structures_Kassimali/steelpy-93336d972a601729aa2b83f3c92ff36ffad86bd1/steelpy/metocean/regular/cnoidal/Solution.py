#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
from array import array
#from dataclasses import dataclass
import math
#from typing import NamedTuple, Tuple, Union, List, Dict

# package imports
#from steelpy.metocean.regular.fourier.Subroutines import zeros
from steelpy.metocean.regular.cnoidal.Elliptic import Elliptic_integrals, cn, sn, dn


# *********************************************************************
# Direct iteration to solve for m, or K in the case of very long waves
# *********************************************************************

def Solve(T: float, H: float, order: int,
          current: float, c_type: int,
          case: str, m: float, m1: float, m1_limit: float,
          iter_limit: int = 20, L1: int = 10., T1: int = 10., accuracy: float = 1.e-4):
    """ """
    case = case.lower()
    pi = math.pi
    K = 1
    if m1 < m1_limit:
        m = 1.
        K = 10.
    #
    print("# Solving for parameter m or K iteratively")
    print("m      K")
    for niter in range(iter_limit + 1):
        print(f"{m:3.4f} {K:3.4f}")
        #
        e, ee, mm, Kd = Elliptic_integrals(m, m1_limit)
        epsilon = H / hoverd(H, e, ee, m, mm, order)
        if case == 'period':
            if c_type == 1:
                K = (T * (current + Ubar_h(epsilon, e, m, mm, order)
                          * math.sqrt(hoverd(H, e, ee, m, mm, order)))
                     * math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order))
            else:
                # if (c_type == 2):
                K = (T * (current + Q_h(epsilon) * pow(hoverd(H, e, ee, m, mm, order), 1.5))
                     * math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order))
        else:
            # elif (Known, Wavelength):
            K = L * math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order)

        if m1 > m1_limit:
            q1 = math.exp(-pi * K / Kd)
            m = pow((1 - 2 * q1 + 2 * pow(q1, 4)) / (1 + 2 * q1 + 2 * pow(q1, 4)), 4)
            e, ee, mm, Kd = Elliptic_integrals(m, m1_limit)

        if case == 'period':
            L = K / (math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order))
            if abs(L / L1 - 1.) < accuracy:
                break
            L1 = L
        else:
            # elif(Known, Wavelength):
            if c_type == 1:
                T = (K / ((current + Ubar_h(epsilon, e, m, mm, order)
                           * math.sqrt(hoverd(H, e, ee, m, mm, order)))
                          * math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order)))
            else:
                # if (c_type == 2):
                T = (K / ((current + Q_h(epsilon) * pow(hoverd(H, e, ee, m, mm, order), 1.5))
                          * math.sqrt(3 * H / m) / 4 / lambda_series(H, e, ee, m, mm, order)))

            if abs(T / T1 - 1.) < accuracy:
                break
            T1 = T

        if niter >= iter_limit:
            raise RuntimeError("Iteration has not converged")
            # print("\nContact John Fenton johndfenton@gmail.com")
    #
    print(f"{m:3.4f} {K:3.4f}")
    return L, T, K, Kd, e, ee, m, mm, q1


#
#
#
#################################
# Series from the cnoidal solution
#################################

#################################################
# Ubar_h ( H/h)
#################################################

def Ubar_h(epsilon: float, e: float, m: float, mm: array, order: int):
    """"""
    U_bar_Height = zeros(7)
    # print(`"U_bar_Height" is U/math.sqrt(g.h) as a function of H/h, m, and e(m), Fenton (1999, eqn A.6)`)
    U_bar_Height[1] = 1 + epsilon / m * (1. / 2 - e)

    U_bar_Height[2] = (U_bar_Height[1] + pow(epsilon, 2) / mm[2]
                       * (-13. / 120 - 1. / 60 * m - 1. / 40 * mm[2] + (1. / 3 + 1. / 12 * m) * e))

    U_bar_Height[3] = (U_bar_Height[2] + pow(epsilon, 3) / mm[3]
                       * (-361. / 2100 + 1899. / 5600 * m - 2689. / 16800 * mm[2]
                          + 13. / 280 * mm[3] + (7. / 75 - 103. / 300 * m + 131. / 600 * mm[2]) * e))

    U_bar_Height[4] = (U_bar_Height[3] + pow(epsilon, 4) / mm[4]
                       * (2349. / 112000 + 29053. / 168000 * m - 1181. / 2100 * mm[2] + 11161. / 28000 * mm[3]
                          - 273. / 3200 * mm[4] + (29369. / 28000 * mm[2] - 15867. / 28000 * mm[
                        3] - 5729. / 8400 * m + 1583. / 4200) * e))

    U_bar_Height[5] = (U_bar_Height[4] + pow(epsilon, 5) / mm[5]
                       * (1786123. / 16170000 - 32376301. / 103488000 * m - 87413873. / 776160000 * mm[2]
                          + 474001783. / 517440000 * mm[3] - 71678951. / 97020000 * mm[4] + 97103. / 616000 * mm[5]
                          + (-61854593. / 35280000 * mm[3] + 35498549. / 35280000 * mm[4] + 444959. / 1260000 * mm[2]
                             + 1196747. / 980000 * m - 691177. / 735000) * e))

    U_bar_Height[6] = Aitken(U_bar_Height, 5)

    return U_bar_Height[order]


#

#################################################
# Q_h (H/h)
#################################################

def Q_h(epsilon: float, m: float, mm: array, order: int):
    """ """
    Q_Height = zeros(7)
    # print("Q_Height" is Q/math.sqrt(g.h^3) as a function of H/h and m, Fenton (1999, eqn A.4))
    Q_Height[1] = 1 + epsilon / m * (-1. / 2 + m)

    Q_Height[2] = Q_Height[1] + pow(epsilon, 2) / mm[2] * (9. / 40 - 7. / 20 * m - 1. / 40 * mm[2])

    Q_Height[3] = (Q_Height[2] + pow(epsilon, 3) / mm[3]
                   * (-11. / 140 + 69. / 1120 * m + 11. / 224 * mm[2] + 3. / 140 * mm[3]))

    Q_Height[4] = (Q_Height[3] + pow(epsilon, 4) / mm[4]
                   * (-16109. / 42000 * mm[3] + 59321. / 84000 * mm[2]
                      - 123787. / 168000 * m - 871. / 22400 * mm[4] + 133687. / 336000))

    Q_Height[5] = (Q_Height[4] + pow(epsilon, 5) / mm[5]
                   * (89101. / 1232000 * mm[5] + 7482007. / 16170000 * mm[4]
                      - 4473257. / 5390000 - 347331631. / 517440000 * mm[3]
                      - 21859819. / 36960000 * mm[2] + 163246841. / 103488000 * m))

    Q_Height[6] = Aitken(Q_Height, 5)
    return Q_Height[order]


#
#################################################
# lambda_d ( H/d)
#################################################

def lambda_d(H: float, K: float, e: float, ee: array,
             m: float, mm: array, order: int):
    """ """
    return K * 4 / math.sqrt(3 * H / m) * lambda_series(H, e, ee, m, mm, order)


#
#
def lambda_series(H: float, e: float, ee: array, m: float, mm: array, order: int):
    """ """
    Wavelength = zeros(7)
    # print(`"Wavelength" is lamda/d as a function of H/d, m, and e(m), Fenton (1999, eqn A.7)`)
    Wavelength[1] = 1
    Wavelength[2] = Wavelength[1] + H / m * (-3. / 2 * e + 5. / 4 - 5. / 8 * m)
    Wavelength[3] = (Wavelength[2] + pow(H, 2) / mm[2]
                     * (-15. / 32 + 15. / 32 * m - 21. / 128 * mm[2] + (-1. / 16 * m + 1. / 8) * e + 3. / 8 * ee[2]))

    Wavelength[4] = (Wavelength[3] + pow(H, 3) / mm[3]
                     * (341227. / 336000 - 341227. / 224000 * m + 984359. / 1344000 * mm[2]
                        - 20127. / 179200 * mm[3] + (-1471. / 1600 - 409. / 6400 * mm[2] + 1471. / 1600 * m) * e
                        + (-7. / 64 * m + 7. / 32) * ee[2] + 1. / 16 * ee[3]))

    Wavelength[5] = (Wavelength[4] + pow(H, 4) / mm[4]
                     * (-105363683. / 37632000 + 105363683. / 18816000 * m
                        - 306621467. / 75264000 * mm[2] + 95894101. / 75264000 * mm[3] - 1575087. / 28672000 * mm[4]
                        + (-2462811. / 448000 * m + 820937. / 224000 - 1086367. / 1792000 * mm[3]
                           + 2728241. / 896000 * mm[2]) * e
                        + (-9501. / 6400 - 2679. / 25600 * mm[2] + 9501. / 6400 * m) * ee[2]
                        + (13. / 64 - 13. / 128 * m) * ee[3] + 3. / 128 * ee[4]))

    Wavelength[6] = Aitken(Wavelength, 5)

    return Wavelength[order]


#
#################################################
# h/d ( H, m)
#################################################

def hoverd(H: float, e: float, ee: array,
           m: float, mm: array, order: int):
    """ """
    hd = zeros(7)
    # double hd[7]
    hd[1] = 1 + H / m * (1 - e - m)
    hd[2] = hd[1] + pow(H / m, 2) * (-1. / 2 + 1. / 2 * m + (1. / 2 - 1. / 4 * m) * e)

    hd[3] = (hd[2] + pow(H / m, 3)
             * (133. / 200 - 399. / 400 * m + 133. / 400 * mm[2]
                + (233. / 200 * m - 1. / 25 * mm[2] - 233. / 200) * e + (1. / 2 - 1. / 4 * m) * ee[2]))

    hd[4] = (hd[3] + pow(H / m, 4)
             * (-122. / 75 + 244. / 75 * m - 1227. / 500 * mm[2] + 1241. / 1500 * mm[3]
                + (481. / 150 + 6529. / 3000 * mm[2] - 573. / 2000 * mm[3] - 481. / 100 * m) * e
                + (52. / 25 * m - 57. / 400 * mm[2] - 52. / 25) * ee[2] + (1. / 2 - 1. / 4 * m) * ee[3]))

    hd[5] = (hd[4] + pow(H / m, 5)
             * (57231077. / 11760000 - 57231077. / 4704000 * m + 69379843. / 5880000 * mm[2]
                - 130123673. / 23520000 * mm[3] + 123967. / 120000 * mm[4]
                + (126350477. / 5880000 * m + 2579201. / 490000 * mm[3] - 302159. / 1470000 * mm[4]
                   - 26893043. / 1680000 * mm[2] - 126350477. / 11760000) * e
                + (24361. / 4000 * mm[2] - 1779. / 2000 * mm[3] - 10347. / 800 * m + 3449. / 400) * ee[2]
                + (-123. / 400 * mm[2] + 649. / 200 * m - 649. / 200) * ee[3] + (1. / 2 - 1. / 4 * m) * ee[4]))

    hd[6] = Aitken(hd, 5)
    return hd[order]


#
#################################################
# alpha ( H/h)
#################################################

def Alpha(epsilon: float, m: float, mm: array, order: int):
    """"""
    alpha = zeros(7)
    # This is the original series for alpha(H/h)
    # print("Alpha" is alpha as a function of H/h and m, Fenton (1999, eqn A.2))
    alpha[1] = 1
    alpha[2] = alpha[1] + epsilon / m * (1. / 4 - 7. / 8 * m)

    alpha[3] = alpha[2] + pow(epsilon, 2) / mm[2] * (1. / 32 - 11. / 32 * m + 111. / 128 * mm[2])

    alpha[4] = (alpha[3] + pow(epsilon, 3) / mm[3]
                * (184711. / 1344000 * mm[2] + 114567. / 224000 * m
                   - 126817. / 336000 - 149273. / 179200 * mm[3]))

    alpha[5] = (alpha[4] + pow(epsilon, 4) / mm[4]
                * (13618217. / 25088000 * mm[3] + 22012297. / 28672000 * mm[4]
                   - 34858533. / 25088000 * mm[2] + 509843. / 2508800 + 2777099. / 6272000 * m))

    alpha[6] = Aitken(alpha, 5)

    for i in range(1, 6 + 1):
        alpha[i] = math.sqrt(3 * epsilon / 4 / m) * alpha[i]
    return alpha[order]


#
#################################################
# R_h ( H/h)
#################################################

def R_h(epsilon: float, m: float, mm: array, order: int):
    """ """
    R_Height = zeros(7)
    # print(`"R_Height" is R/(g.h) as a function of H/h and m, Fenton (1999, eqn A.5)`)
    R_Height[1] = 3. / 2 + epsilon / m * (-1. / 2 + m)

    R_Height[2] = (R_Height[1] + pow(epsilon, 2) / mm[2]
                   * (-7. / 20 * m + 7. / 20 - 1. / 40 * mm[2]))

    R_Height[3] = (R_Height[2] + pow(epsilon, 3) / mm[3]
                   * (25. / 224 * m - 107. / 560 + 13. / 1120 * mm[2] + 13. / 280 * mm[3]))

    R_Height[4] = (R_Height[3] + pow(epsilon, 4) / mm[4]
                   * (-30823. / 42000 * m + 55331. / 84000 * mm[2]
                      + 1214. / 2625 - 26833. / 84000 * mm[3] - 17. / 200 * mm[4]))

    R_Height[5] = (R_Height[4] + pow(epsilon, 5) / mm[5]
                   * (-270759631. / 258720000 + 24097. / 154000 * mm[5] + 21098053. / 64680000 * mm[4]
                      - 202951241. / 517440000 * mm[3] - 864417. / 880000 * mm[2] + 198968527. / 103488000 * m))

    R_Height[6] = Aitken(R_Height, 5)
    return R_Height[order]


#
#
#####################
# eta_h (x/h)
#####################

def eta_h(x: float, alpha: float, epsilon: float,
          m: float, mm: array, m1: float, m1_limit: float,
          q1: float, K: float, Kd: float, order: int):
    """ """
    C = zeros(11)
    Eta = zeros(7)
    # print("Eta" is eta/h as a function of H/h, m, and cn^2, Fenton (1999, eqn A.1));
    #
    # Correction to 'm' from previous 'k' advised by Thomas Lykke Andersen
    C[1] = cn(alpha * x, m, m1, m1_limit, q1, K, Kd)
    for i in range(2, 10 + 1):
        C[i] = C[i - 1] * C[1]
    #
    Eta[1] = 1 + C[2] * epsilon

    Eta[2] = Eta[1] + pow(epsilon, 2) / mm[2] * (-3. / 4 * mm[2] * C[2] + 3. / 4 * mm[2] * C[4])

    Eta[3] = (Eta[2] + pow(epsilon, 3) / mm[3]
              * ((111. / 80 * mm[3] - 61. / 80 * mm[2]) * C[2]
                 + (-53. / 20 * mm[3] + 61. / 80 * mm[2]) * C[4] + 101. / 80 * mm[3] * C[6]))

    Eta[4] = (Eta[3] + pow(epsilon, 4) / mm[4]
              * ((59737. / 24000 * mm[3] - 4883. / 1600 * mm[4] - 302. / 375 * mm[2]) * C[2]
                 + (-20791. / 4800 * mm[3] + 302. / 375 * mm[2] + 35551. / 4800 * mm[4]) * C[4]
                 + (22109. / 12000 * mm[3] - 156611. / 24000 * mm[4]) * C[6] + 17367. / 8000 * mm[4] * C[8]))

    Eta[5] = (Eta[4] + pow(epsilon, 5) / mm[5]
              * ((209511. / 32000 * mm[5] - 2209587. / 313600 * mm[4] + 3014947. / 1568000 * mm[3]
                  + 684317. / 1568000 * mm[2]) * C[2]
                 + (-2218593. / 112000 * mm[5] - 684317. / 1568000 * mm[2] + 3910057. / 224000 * mm[4]
                    - 114211. / 24500 * mm[3]) * C[4]
                 + (-490143. / 32000 * mm[4] + 40547. / 1600 * mm[5] + 4294557. / 1568000 * mm[3]) * C[6]
                 + (-12800441. / 784000 * mm[5] + 7694543. / 1568000 * mm[4]) * C[8]
                 + 1331817. / 313600 * mm[5] * C[10]))

    Eta[6] = Aitken(Eta, 5)
    return Eta[order]


#
#####################
# u_h (x/h, y/h)
#####################

def u_h(x, Y, alpha, delta, m, mm, m1, m1_limit, q1, K, Kd, order):
    """ """
    C = zeros(11)
    y = zeros(11)
    uu = zeros(7)
    # 
    # print("uu" is U/math.sqrt(g.h) as a function of delta, m, y/h and cn^2, Fenton (1999, eqn A.3.1))
    # Correction to 'm' from previous 'k' advised by Thomas Lykke Andersen
    C[1] = cn(alpha * x, m, m1, m1_limit, q1, K, Kd)
    y[1] = Y
    for i in range(2, 10 + 1):
        C[i] = C[i - 1] * C[1]
        y[i] = y[i - 1] * y[1]

    uu[1] = -1 + (1. / 2 - m + m * C[2]) * delta
    uu[2] = (uu[1] + pow(delta, 2)
             * (-79. / 40 * mm[2] - 19. / 40 + 79. / 40 * m + C[2] * (-3. / 2 * m + 3 * mm[2]) - mm[2] * C[4]
                + (-3. / 4 * m + 3. / 4 * mm[2] + C[2] * (-3 * mm[2] + 3. / 2 * m) + 9. / 4 * mm[2] * C[4]) * y[2]))

    uu[3] = (uu[2] + pow(delta, 3)
             * (55. / 112 + 7113. / 1120 * mm[2] - 2371. / 560 * mm[3] - 3471. / 1120 * m
                + C[2] * (71. / 40 * m - 339. / 40 * mm[2] + 339. / 40 * mm[3])
                + C[4] * (27. / 10 * mm[2] - 27. / 5 * mm[3]) + 6. / 5 * mm[3] * C[6]
                + (9. / 8 * m - 27. / 8 * mm[2] + 9. / 4 * mm[3] + C[2] * (
                                -27. / 2 * mm[3] - 9. / 4 * m + 27. / 2 * mm[2])
                   + C[4] * (-75. / 8 * mm[2] + 75. / 4 * mm[3]) - 15. / 2 * mm[3] * C[6]) * y[2]
                + (-3. / 8 * mm[3] - 3. / 16 * m + 9. / 16 * mm[2] + C[2]
                   * (3. / 8 * m + 51. / 16 * mm[3] - 51. / 16 * mm[2])
                   + C[4] * (-45. / 8 * mm[3] + 45. / 16 * mm[2]) + 45. / 16 * mm[3] * C[6]) * y[4]))

    uu[4] = (uu[3] + pow(delta, 4)
             * (-11813. / 22400 - 382841. / 28000 * mm[2] + 108923. / 5600 * mm[3]
                - 108923. / 11200 * mm[4] + 31581. / 8000 * m
                + C[2] * (-53327. / 42000 * m + 1192733. / 84000 * mm[2] - 39177. / 1120 * mm[3] + 13059. / 560 * mm[4])
                + C[4] * (-13109. / 3000 * mm[2] + 12793. / 600 * mm[3] - 12793. / 600 * mm[4])
                + C[6] * (-1763. / 375 * mm[3] + 3526. / 375 * mm[4]) - 197. / 125 * mm[4] * C[8]
                + (1017. / 160 * mm[4] - 213. / 160 * m + 123. / 16 * mm[2] - 1017. / 80 * mm[3]
                   + C[2] * (5967. / 80 * mm[3] + 213. / 80 * m - 483. / 16 * mm[2] - 1989. / 40 * mm[4])
                   + C[4] * (3231. / 160 * mm[2] - 15579. / 160 * mm[3] + 15579. / 160 * mm[4])
                   + C[6] * (729. / 20 * mm[3] - 729. / 10 * mm[4]) + 189. / 10 * mm[4] * C[8]) * y[2]
                + (-27. / 16 * mm[4] + 27. / 8 * mm[3] + 9. / 32 * m - 63. / 32 * mm[2]
                   + C[2] * (-9. / 16 * m - 999. / 32 * mm[3] + 369. / 32 * mm[2] + 333. / 16 * mm[4])
                   + C[4] * (453. / 8 * mm[3] - 327. / 32 * mm[2] - 453. / 8 * mm[4])
                   + C[6] * (-915. / 32 * mm[3] + 915. / 16 * mm[4]) - 315. / 16 * mm[4] * C[8]) * y[4]
                + (-3. / 160 * m + 57. / 320 * mm[2] + 51. / 320 * mm[4] - 51. / 160 * mm[3]
                   + C[2] * (279. / 80 * mm[3] - 93. / 40 * mm[4] + 3. / 80 * m - 99. / 80 * mm[2])
                   + C[4] * (567. / 80 * mm[4] - 567. / 80 * mm[3] + 189. / 160 * mm[2])
                   + C[6] * (-63. / 8 * mm[4] + 63. / 16 * mm[3]) + 189. / 64 * mm[4] * C[8]) * y[6]))

    uu[5] = (uu[4] + pow(delta, 5)
             * (57159. / 98560 + 327236467. / 17248000 * mm[2] - 884845613. / 17248000 * mm[3]
                - 57144683. / 2464000 * mm[5]
                + 57144683. / 985600 * mm[4] - 124831351. / 34496000 * m
                + C[2] * (-144821. / 156800 * m - 34543. / 3136 * mm[2] + 14639941. / 196000 * mm[3]
                          - 3566001. / 28000 * mm[4] + 3566001. / 56000 * mm[5])
                + C[4] * (1131733. / 294000 * mm[2] - 26486863. / 588000 * mm[3] + 3137133. / 28000 * mm[4]
                          - 1045711. / 14000 * mm[5])
                + C[6] * (757991. / 73500 * mm[3] - 72731. / 1500 * mm[4] + 72731. / 1500 * mm[5])
                + C[8] * (298481. / 36750 * mm[4] - 298481. / 18375 * mm[5]) + 13438. / 6125 * mm[5] * C[10]
                + (-39177. / 896 * mm[4] + 39177. / 2240 * mm[5] + 53327. / 56000 * m
                   - 1299387. / 112000 * mm[2] + 9221. / 250 * mm[3]
                   + C[2] * (-11797957. / 56000 * mm[3] - 53327. / 28000 * m + 358171. / 8000 * mm[2]
                             - 232269. / 1400 * mm[5] + 232269. / 700 * mm[4])
                   + C[4] * (-1628189. / 56000 * mm[2] + 29702871. / 112000 * mm[3]
                             + 4638023. / 11200 * mm[5] - 13914069. / 22400 * mm[4])
                   + C[6] * (-192481. / 2000 * mm[3] - 893761. / 2000 * mm[5] + 893761. / 2000 * mm[4])
                   + C[8] * (11187. / 50 * mm[5] - 11187. / 100 * mm[4]) - 5319. / 125 * mm[5] * C[10]) * y[2]
                + (1989. / 128 * mm[4] - 1989. / 320 * mm[5] - 4191. / 320 * mm[3] - 213. / 640 * m + 657. / 160 * mm[2]
                   + C[2] * (213. / 320 * m + 9753. / 80 * mm[3] - 3075. / 128 * mm[2] + 62649. / 640 * mm[
                                5] - 62649. / 320 * mm[4])
                   + C[4] * (-139149. / 640 * mm[3] + 13563. / 640 * mm[2] - 112023. / 320 * mm[5] + 336069. / 640 * mm[
                                4])
                   + C[6] * (68643. / 640 * mm[3] + 330183. / 640 * mm[5] - 330183. / 640 * mm[4])
                   + C[8] * (-5481. / 16 * mm[5] + 5481. / 32 * mm[4]) + 1701. / 20 * mm[5] * C[10]) * y[4]
                + (9. / 320 * m - 387. / 640 * mm[2] - 333. / 128 * mm[4] + 171. / 80 * mm[3] + 333. / 320 * mm[5]
                   + C[2] * (-423. / 20 * mm[5] - 4077. / 160 * mm[3] + 423. / 10 * mm[4] - 9. / 160 * m + 693. / 160 *
                             mm[2])
                   + C[4] * (1461. / 16 * mm[5] - 4383. / 32 * mm[4] + 54 * mm[3] - 267. / 64 * mm[2])
                   + C[6] * (-2541. / 16 * mm[5] + 2541. / 16 * mm[4] - 987. / 32 * mm[3])
                   + C[8] * (7875. / 64 * mm[5] - 7875. / 128 * mm[4]) - 567. / 16 * mm[5] * C[10]) * y[6]
                + (-9. / 8960 * m + 153. / 4480 * mm[2] - 279. / 4480 * mm[5] - 81. / 640 * mm[3] + 279. / 1792 * mm[4]
                   + C[2] * (14769. / 8960 * mm[3] - 333. / 1280 * mm[2] + 6219. / 4480 * mm[5] - 6219. / 2240 * mm[
                                4] + 9. / 4480 * m)
                   + C[4] * (4293. / 448 * mm[4] - 3321. / 896 * mm[3] - 1431. / 224 * mm[5] + 459. / 1792 * mm[2])
                   + C[6] * (567. / 256 * mm[3] + 2997. / 256 * mm[5] - 2997. / 256 * mm[4])
                   + C[8] * (-1215. / 128 * mm[5] + 1215. / 256 * mm[4]) + 729. / 256 * mm[5] * C[10]) * y[8]))

    uu[6] = uu[5]
    # NB - the Aitken velocities were a bit irregular, so I did not apply them
    return uu[order]


#
#####################
# v_h (delta, x, y)
#####################

def v_h(x, Y, alpha, delta, m, mm, m1, m1_limit, q1, K, Kd, order):
    """ """
    C = zeros(11)
    y = zeros(11)
    vv = zeros(7)
    #
    # Correction to 'm' from previous 'k' advised by Thomas Lykke Andersen
    C[1] = cn(alpha * x, m, m1, m1_limit, q1, K, Kd)
    y[1] = Y
    for i in range(2, 10 + 1):
        C[i] = C[i - 1] * C[1]
        y[i] = y[i - 1] * y[1]

    S = sn(alpha * x, m, m1, m1_limit, q1, K, Kd)
    D = dn(alpha * x, m, m1, m1_limit, q1, K, Kd)

    Lead = y[1] * m * C[1] * S * D * math.sqrt(3) * pow(delta, 3. / 2)

    vv[1] = 1
    vv[2] = vv[1] + ((1. / 2 - m + (3. / 2) * m * C[2]) * y[2] - 2 * m * C[2] + 3 * m - 3. / 2) * delta

    vv[3] = (vv[2] + (((27. / 16) * mm[2] * C[4]
                       + ((9. / 8) * m - (9. / 4) * mm[2]) * C[2] - (51. / 80) * m + (51. / 80) * mm[2] + 3. / 40) * y[
                          4]
                      + (-(15. / 2) * mm[2] * C[4] + (-(25. / 4) * m + (25. / 2) * mm[2]) * C[2] - 3. / 4 - (9. / 2) *
                         mm[2]
                         + (9. / 2) * m) * y[2] + (18. / 5) * mm[2] * C[4] + ((27. / 5) * m - (54. / 5) * mm[2]) * C[2]
                      + (339. / 40) * mm[2] + 71. / 40 - (339. / 40) * m) * pow(delta, 2))

    vv[4] = (vv[3] + (((27. / 16) * mm[3] * C[6] + (-(27. / 8) * mm[3] + (27. / 16) * mm[2]) * C[4]
                       + (-(81. / 40) * mm[2] + (81. / 40) * mm[3] + (27. / 80) * m) * C[2] - (99. / 560) * m
                       + (279. / 560) * mm[2] + 3. / 560 - (93. / 280) * mm[3]) * y[6]
                      + (-(63. / 4) * mm[3] * C[6] + (-(549. / 32) * mm[2] + (549. / 16) * mm[3]) * C[4]
                         + (-(327. / 80) * m - (453. / 20) * mm[3] + (453. / 20) * mm[2]) * C[2] - (999. / 160) * mm[2]
                         + (333. / 80) * mm[3] - 9. / 80 + (369. / 160) * m) * y[4]
                      + ((126. / 5) * mm[3] * C[6] + ((729. / 20) * mm[2] - (729. / 10) * mm[3]) * C[4]
                         + ((5193. / 80) * mm[3] + (1077. / 80) * m - (5193. / 80) * mm[2]) * C[2]
                         + (1989. / 80) * mm[2] - (161. / 16) * m - (663. / 40) * mm[3] + 71. / 80) * y[2]
                      - (788. / 125) * mm[3] * C[6] + (-(1763. / 125) * mm[2] + (3526. / 125) * mm[3]) * C[4]
                      + (-(12793. / 300) * mm[3] - (13109. / 1500) * m + (12793. / 300) * mm[2]) * C[2]
                      + (1192733. / 84000) * m - (39177. / 1120) * mm[2] + (13059. / 560) * mm[3]
                      - 53327. / 42000) * pow(delta, 3))

    vv[5] = (vv[4] + (((405. / 256) * mm[4] * C[8] + ((135. / 64) * mm[3] - (135. / 32) * mm[4]) * C[6]
                       + ((999. / 256) * mm[4] + (189. / 256) * mm[2] - (999. / 256) * mm[3]) * C[4]
                       + (-(369. / 448) * mm[2] + (477. / 224) * mm[3] + (51. / 896) * m - (159. / 112) * mm[4]) * C[2]
                       + (691. / 4480) * mm[4] + 1. / 4480 - (37. / 1280) * m - (691. / 2240) * mm[3] + (1641. / 8960) *
                       mm[2]) * y[8]
                      + (-(405. / 16) * mm[4] * C[8] + (-(1125. / 32) * mm[3] + (1125. / 16) * mm[4]) * C[6]
                         + (-(423. / 32) * mm[2] + (1089. / 16) * mm[3] - (1089. / 16) * mm[4]) * C[4]
                         + ((108. / 7) * mm[2] - (4383. / 112) * mm[3] - (267. / 224) * m + (1461. / 56) * mm[4]) * C[2]
                         - 9. / 1120 + (99. / 160) * m - (423. / 140) * mm[4] - (4077. / 1120) * mm[2] + (423. / 70) *
                         mm[3]) * y[6]
                      + ((1701. / 20) * mm[4] * C[8] + ((5481. / 40) * mm[3] - (5481. / 20) * mm[4]) * C[6]
                         + ((990549. / 3200) * mm[4] + (205929. / 3200) * mm[2] - (990549. / 3200) * mm[3]) * C[4]
                         + ((13563. / 1600) * m - (112023. / 800) * mm[4] + (336069. / 1600) * mm[3]
                            - (139149. / 1600) * mm[2]) * C[2] + (9753. / 400) * mm[2] - (62649. / 1600) * mm[3]
                         - (615. / 128) * m + (62649. / 3200) * mm[4] + 213. / 1600) * y[4]
                      + (-(1773. / 25) * mm[4] * C[8] + (-(3729. / 25) * mm[3] + (7458. / 25) * mm[4]) * C[6]
                         + ((893761. / 2000) * mm[3] - (893761. / 2000) * mm[4] - (192481. / 2000) * mm[2]) * C[4]
                         + (-(4638023. / 11200) * mm[3] + (4638023. / 16800) * mm[4] - (1628189. / 84000) * m
                            + (9900957. / 56000) * mm[2]) * C[2] + (358171. / 24000) * m - (11797957. / 168000) * mm[2]
                         - (77423. / 1400) * mm[4] - 53327. / 84000 + (77423. / 700) * mm[3]) * y[2] + (13438. / 1225) *
                      mm[4] * C[8]
                      + (-(1193924. / 18375) * mm[4] + (596962. / 18375) * mm[3]) * C[6]
                      + ((757991. / 24500) * mm[2] - (72731. / 500) * mm[3] + (72731. / 500) * mm[4]) * C[4]
                      + ((3137133. / 14000) * mm[3] - (26486863. / 294000) * mm[2] + (1131733. / 147000) * m
                         - (1045711. / 7000) * mm[4]) * C[2] - (34543. / 3136) * m + (14639941. / 196000) * mm[2]
                      - (3566001. / 28000) * mm[3] + (3566001. / 56000) * mm[4] - 144821. / 156800) * pow(delta, 4))
    vv[6] = vv[5]
    # NB - the Aitken velocities were a bit irregular, so I did not apply them
    return vv[order] * Lead


#

#########################
# Convergence enhancement
#########################
#
# Function to calculate Aitken transform
def Aitken(S: array, j: int):
    """ """
    den = S[j] + S[j - 2] - 2 * S[j - 1]
    if abs(den) < 1e-6:
        return S[j]
    else:
        return S[j] - pow(S[j] - S[j - 1], 2) / den
    # return R
#
# def zeros(m, n=False, code='d'):
#    """
#    Create zero matrix
#    """
#    if n:
#        new_matrix = [array(code, [0 for row in range(n)]) for col in range(m)]
#
#    else:
#        new_matrix = array(code, [0 for row in range(m)])
#
#    return new_matrix
#
#
