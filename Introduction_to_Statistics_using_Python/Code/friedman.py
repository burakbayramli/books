from scipy import stats

# Generate the data
nd = stats.norm(5,3)
data = nd.rvs((100,3))

# Check if the three are from the same distribution
alpha = 0.05
_,p = stats.friedmanchisquare(data[:,0], data[:,1], data[:,2])
if p > alpha:
    print 'First datasets come from the same distribution'

# Modify one of the data
data[:,1] += 3
_,p = stats.friedmanchisquare(data[:,0], data[:,1], data[:,2])
if p < alpha:
    print 'After the modification, one of the datasets is different'

