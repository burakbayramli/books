'''
Stability of the T-distribution against outliers, compared to the normal distribution.

'''

# author: Thomas Haslwanter, date: March-2014

import numpy as np
import scipy.stats as ss
import matplotlib.pyplot as plt

# Generate the data
np.random.seed(12345)
ndata = 100
data = ss.norm.rvs(size=(ndata,))
data2 = ss.norm.rvs(5, size=(20,))
dataWOutlier = np.hstack((data, data2))

# Calculate the fits
fit_gauss_wo = ss.norm.fit(data)
fit_gauss_w = ss.norm.fit(dataWOutlier)

fit_t_wo = ss.t.fit(data)
fit_t_w = ss.t.fit(dataWOutlier)

# Fitted curves
fitted_x = np.linspace(-7, 7, 200)

fitted_gauss_wo = ss.norm.pdf(fitted_x, loc=fit_gauss_wo[0], scale=fit_gauss_wo[1])
fitted_t_wo = ss.t.pdf(fitted_x, df=ndata-1, loc=fit_t_wo[1], scale=fit_t_wo[2])

fitted_gauss_w = ss.norm.pdf(fitted_x, loc=fit_gauss_w[0], scale=fit_gauss_w[1])
fitted_t_w = ss.t.pdf(fitted_x, df=ndata-1, loc=fit_t_w[1], scale=fit_t_w[2])

# Fit normal and t-distribution to both datasets, and display the results
fit_wo = ss.norm.fit(data)
fit_w = ss.norm.fit(dataWOutlier)
print('Gaussian distribution: {0} -> {1}'.format(fit_gauss_wo, fit_gauss_w))

# Fit of "df" is not shown, as it is pretty unstable for large df
print('T-distribution: {0} -> {1}'.format(fit_t_wo[1:], fit_t_w[1:]))

# Show the data
fig, axs = plt.subplots(2,1, sharex=True)
axs[0].hist(data, normed=True, bins=25)
axs[0].plot(fitted_x, fitted_gauss_wo, label='normal')
axs[0].plot(fitted_x, fitted_t_wo, ls='--', lw=2, label='t-dist')
axs[0].set_title('Without outliers')
axs[0].legend()

axs[1].hist(dataWOutlier, normed=True, bins=25)
axs[1].plot(fitted_x, fitted_gauss_w, label='norm')
axs[1].plot(fitted_x, fitted_t_w, ls='--', lw=2, label='t-dist')
axs[1].set_title('With outliers')

outFile = r'C:\Temp\ttest_stability.png'
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))
plt.show()

