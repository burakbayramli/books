import numpy
from matplotlib import pyplot as plt
import statsmodels.api as sm
from scipy import stats
    
def converter(x):
    if x == 'NA':
        return numpy.nan
    else:
        return float(x) 

# read data
heights = numpy.loadtxt("heights.dat",  skiprows=1, usecols = (1,2,3,4,5,6,7,8,9),
                    converters={1:converter,2:converter,3:converter,9:converter})

age = 90 - heights[:,7]
age[age<18] = numpy.nan
age_category = numpy.zeros(len(age))

age_category[(age<35)] = 1
age_category[(age>=35) & (age<50)] = 2
age_category[age>=50] = 3

race = heights[:,4]
hisp = heights[:,5]
eth = numpy.zeros(len(age))
eth[(race != 2) & (hisp != 1) & (race == 1)] = 3
eth[(race != 2) & (hisp != 1) & (race != 1)] = 4
eth[(race != 2) & (hisp == 1)] = 2
eth[(race == 2)] = 1

sex = heights[:,3]
male = 2 - sex

new_heights = numpy.zeros((len(eth),10))
new_heights[:,0] = heights[:,0] # earn
new_heights[:,1] = heights[:,8] # height
new_heights[:,2] = heights[:,3] # sex
new_heights[:,3] = heights[:,4] # race
new_heights[:,4] = heights[:,5] # hisp
new_heights[:,5] = heights[:,6] # ed
new_heights[:,6] = age
new_heights[:,7] = age_category
new_heights[:,8] = eth
new_heights[:,9] = male

new_heights = new_heights[(numpy.isnan(new_heights[:,0]) == False) & 
                          (numpy.isnan(new_heights[:,1]) == False) & 
                          (numpy.isnan(new_heights[:,2]) == False) & 
                          (numpy.isnan(new_heights[:,6]) == False) &
                          (new_heights[:,0] > 0) &
                          (heights[:,7] > 25)]
                          
print len(new_heights)

log_earn = numpy.log(new_heights[:,0])
         
print sm.add_constant(new_heights[:,8])
print log_earn

# log(earn) ~ height
olsmod = sm.OLS(log_earn, sm.add_constant(new_heights[:,1]))
olsres = olsmod.fit()
print olsres.summary()

