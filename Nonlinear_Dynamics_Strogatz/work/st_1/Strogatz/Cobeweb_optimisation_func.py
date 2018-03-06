import numba as nb
import numpy as np
@nb.jit(nopython=True)
def cob(result):
    horizontal = [result[0]]
    vertical = [result[0]]
    for x in result[1:]:
        horizontal.append(vertical[-1])
        vertical.append(x)
        horizontal.append(x)
        vertical.append(x)
        return horizontal, vertical
results= np.linspace(0,10,1000)
cob(results)