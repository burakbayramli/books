import random
random.seed(42)
N = 500  # no of samples
x = range(N)
y = [random.uniform(-1,1) for i in x]

import scitools.std as st
st.plot(x, y, '+', axis=[0,N-1,-1.2,1.2])
st.hardcopy('tmp.eps')

import matplotlib.pyplot as plt
plt.plot(x, y, '+')
plt.axis([0,N-1,-1.2,1.2])
plt.savefig('tmp.eps')
plt.show()
