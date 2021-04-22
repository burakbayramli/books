# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017-2018, Petr Krysl
#

"""
This script will create a uniaxial carbon/epoxy lamina material.
"""

from material import *
TheModel = getInput('Enter the model name:')
if TheModel != '':
    mdb.models[TheModel].Material(description='AS4D/9310 in SI(mm) units', name='AS4D--9310')
    mdb.models[TheModel].materials['AS4D--9310'].Elastic(table=(
    (1.3386e5, 7.706e3, 0.301, 4.306e3, 4.306e3, 2.76e3),), type=LAMINA)
