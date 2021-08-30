#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

sig=1e6*np.array([[2.5,2.5,0],[2.5,2.5,0],[0,0,0]])
[principalstresses, directions]=np.linalg.eig(sig)

# the columns in 'directions' correspond to the dot product of the
# principal axes with the orignal coordinate system
# The rotation angles are obtained by calculating the inverse cosines

theta=np.arccos(directions)*180/(np.pi)
print ('theta=\n', theta)
print ('principal stresses=\n', principalstresses)

