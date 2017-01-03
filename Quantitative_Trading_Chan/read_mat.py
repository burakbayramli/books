# to save a matlab matrix
# save('dosya','A')
from scipy import io as spio
import pandas as pd
a = spio.loadmat('dosya')['A'][0]
