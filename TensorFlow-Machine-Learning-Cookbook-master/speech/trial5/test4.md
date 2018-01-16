
```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

data = np.random.randn(1,10).reshape(10)
print data

shift = 3
pad = data[0]
data[shift:-1] = data[0:len(data)-shift-1] 
data[0:shift] = pad
print data


```

```text
[ 0.58339156 -0.07744316  0.33043413 -0.70866502  0.38029396  0.2912748
 -0.86444312 -1.42068388  1.19971331 -1.57871455]
[ 0.58339156  0.58339156  0.58339156  0.58339156 -0.07744316  0.33043413
 -0.70866502  0.38029396  0.2912748  -1.57871455]
```














