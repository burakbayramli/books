# Pydgeon - the Python DG Environment
# (C) 2009, 2010 Tim Warburton, Xueyu Zhu, Andreas Kloeckner
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.




from __future__ import division

import numpy as np

NODETOL = 1e-12
eps = np.finfo(float).eps




def eldot(mat, vec):
    return np.tensordot(vec, mat, axes=(1,1))

def make_obj_array(res_list):
    """Turn a list into an object array, without recursively turning
    all sub-arrays into object arrays, like numpy.array would do.
    """

    result = np.empty((len(res_list),), dtype=object)
    for i, v in enumerate(res_list):
        result[i] = v

    return result

def fact(z):
    g = 1
    for i in range(1, np.int32(z)):
        g = g*i

    return g
