#!/usr/bin/env python

# Do a check for the python version and print a helpful message.
import sys
if sys.version_info < (2,6):
    sys.exit("Upgrade python to at least version 2.6")

# If the 'numpy' module is not present, print a helpful message.
try:
    import numpy
except:
    sys.exit("Could not find required module 'numpy'.\n"
            "See numpy.scipy.org for more information.")

import math

#
# Helper functions for uniform printing throughout the script.
#

def headerprint(string):
    """ Prints a centered string to divide output sections. """
    mywidth = 64
    mychar = "="
    numspaces = mywidth - len(string)
    before = int(math.ceil(float(mywidth-len(string))/2))
    after  = int(math.floor(float(mywidth-len(string))/2))
    print("\n"+before*mychar+string+after*mychar+"\n")

def valprint(string, value):
    """ Ensure uniform formatting of scalar value outputs. """
    print("{0:>30}: {1: .10e}".format(string, value))

def matprint(string, value):
    """ Ensure uniform formatting of matrix value outputs. """
    print("{0}:".format(string))
    print(value)

def usage():
    """ When the user needs help, print the script usage. """
    headerprint(" Analyze Stress State ")
    s = " For a given stress state this script computes many\n"      + \
        " useful quantities that help to analyze the stress state.\n"+ \
        "\n"                                                         + \
        " Currently, the following values are output:\n"             + \
        "   Isotropic Matrix\n"                                      + \
        "   Deviatoric Matrix\n"                                     + \
        "   Principal Stresses\n"                                    + \
        "   Maximum Shear\n"                                         + \
        "   Mean Stress\n"                                           + \
        "   Equivalent Stress\n"                                     + \
        "   Invariant I1\n"                                          + \
        "   Invariant J2\n"                                          + \
        "   Invariant J3\n"                                          + \
        "   Lode Coordinates\n"                                      + \
        "   Triaxiality\n"                                           + \
        "\n"                                                         + \
        " Command line syntax option 1:\n"                           + \
        "\n"                                                         + \
        "     > ./script sig11 sig22 sig33\n"                        + \
        "\n"                                                         + \
        " Command line syntax option 2:\n"                           + \
        "\n"                                                         + \
        "     > ./script sig11 sig22 sig33 sig12 sig13 sig23\n"
    sys.exit(s)

#
# The main section of the scrip
#
if __name__ == '__main__':
    # If the script is run with an incorrect number of arguments
    # or if the user is asking for help, print the usage information.
    if (len(sys.argv) != 4 and len(sys.argv) != 7) or \
       ("--help" in sys.argv or "-h" in sys.argv):
        usage()

    # load stress components from the command line in a temporary
    # container
    dum = [0.0,0.0,0.0,0.0,0.0,0.0]
    for idx in range(1,len(sys.argv)):
        try:
            dum[idx-1] = float(sys.argv[idx])
        except:
            sys.exit("Argument '{0}' is not a valid float".\
                     format(sys.argv[idx]))

    # load the stresses into our matrix and compute the 
    # deviatoric and isotropic stress matricies
    sigma = numpy.array([[dum[0],dum[3],dum[4]],
                         [dum[3],dum[1],dum[5]],
                         [dum[4],dum[5],dum[2]]])

    sigma_iso = 1.0/3.0*numpy.trace(sigma)*numpy.eye(3)
    sigma_dev = sigma - sigma_iso

    # compute principal stresses
    eigvals = list(numpy.linalg.eigvalsh(sigma))
    eigvals.sort()
    eigvals.reverse()

    # compute max shear stress
    maxshear = (max(eigvals)-min(eigvals))/2.0

    # compute the stress invariants
    I1 = numpy.trace(sigma)
    J2 = 1.0/2.0*numpy.trace(numpy.dot(sigma_dev,sigma_dev))
    J3 = 1.0/3.0*numpy.trace(\
         numpy.dot(sigma_dev,numpy.dot(sigma_dev,sigma_dev)))

    # compute other common stress measures
    mean_stress = 1.0/3.0*I1
    eqv_stress  = math.sqrt(3.0*J2)

    # compute lode coordinates
    lode_r = math.sqrt(2.0*J2)
    lode_z = I1/math.sqrt(3.0)

    dum = 3.0*math.sqrt(6.0)*numpy.linalg.det(sigma_dev/lode_r)
    lode_theta = 1.0/3.0*math.asin(dum)

    # compute the stress triaxiality
    triaxiality = mean_stress/eqv_stress

    # Print out what we've found
    headerprint(" Stress State Analysis ")
    matprint("Input Stress",sigma)
    headerprint(" Component Matricies ")
    matprint("Isotropic Stress",sigma_iso)
    matprint("Deviatoric Stress",sigma_dev)
    headerprint(" Scalar Values ")
    valprint("P1",eigvals[0])
    valprint("P2",eigvals[1])
    valprint("P3",eigvals[2])
    valprint("Max Shear", maxshear)
    valprint("Mean Stress",mean_stress)
    valprint("Equivalent Stress", eqv_stress)
    valprint("I1",I1)
    valprint("J2",J2)
    valprint("J3",J3)
    valprint("Lode z",lode_z)
    valprint("Lode r",lode_r)
    valprint("Lode theta (rad)",lode_theta)
    valprint("Lode theta (deg)",math.degrees(lode_theta))
    valprint("Triaxiality",triaxiality)
    headerprint(" End Output ")

    # End of script