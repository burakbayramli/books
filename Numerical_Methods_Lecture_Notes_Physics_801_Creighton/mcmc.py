import pylab, random, math, mpl_toolkits.mplot3d

# input parameters
N = input('number of points -> ')
Mtrue = input('transition point -> ')
mu1true = input('mean of first stage -> ')
mu2true = input('mean of second stage -> ')
seed = input('random seed -> ')
random.seed(seed)

# generate random data with fixed uncertainty
sigma = 1.0
x = [random.gauss(mu1true, sigma) for i in range(Mtrue)]
x += [random.gauss(mu2true, sigma) for i in range(Mtrue, N)]

# cumulative sum of x used to compute likelihoods
S = [sum(x[:i]) for i in range(N)]

# set up lists to store the steps
nburn = 1000  # number steps during burn-in phase
nmeas = 100000  # number of measurement steps after burn-in
M = [0]*(nburn+nmeas)
mu1 = [0.0]*(nburn+nmeas)
mu2 = [0.0]*(nburn+nmeas)

# perform MCMC
dM = int(0.1*N+0.5)  # step size for M
dmu = 1.0  # step size for mu1 and mu2
M[0] = N//2  # initial guess for transition point
accept = 0
for step in range(1, nburn+nmeas):
    M[step] = M[step-1]+random.randint(-dM, dM)
    mu1[step] = random.gauss(mu1[step-1], dmu)
    mu2[step] = random.gauss(mu2[step-1], dmu)
    logLambda = mu1[step]*S[M[step]-1]+mu2[step]*(S[N-1]-S[M[step]])
    logLambda -= 0.5*(M[step]*mu1[step]**2+(N-M[step])*mu2[step]**2)
    logLambda -= mu1[step-1]*S[M[step-1]-1]+mu2[step-1]*(S[N-1]-S[M[step-1]])
    logLambda += 0.5*(M[step-1]*mu1[step-1]**2+(N-M[step-1])*mu2[step-1]**2)
    logLambda /= sigma**2
    if M[step] < 0 or M[step] >= N:  # reject step if M is out of bounds
        logalpha = -float('inf')
    else:
        logalpha = min(0, logLambda)
    if logalpha >= 0 or math.log(random.random()) < logalpha:  # accept step
        accept += 1
    else:
          # reject step: reset parameters to previous values
        M[step] = M[step-1]
        mu1[step] = mu1[step-1]
        mu2[step] = mu2[step-1]
    # during burn-in phase, adjust step sizes to control rejection rate
    if step < nburn and step%20 == 0:  # check every 20 steps
        rate = accept/20.0  # acceptance rate
        if rate < 0.1:  # rate too low: take smaller steps
            dmu *= 0.5
            dM = 1+int(dM//2)
        if rate > 0.5:  # rate too high: take bigger steps
            dmu *= 2.0
            dM = 2*dM
        accept = 0

# plot data
pylab.figure()
pylab.errorbar(range(N), x, yerr=sigma, fmt='o')
pylab.ylabel('x')
pylab.xlabel('sample')

# plot traces
nplot = 5000  # number of points after burn-in to plot
pylab.figure()
pylab.subplot(2, 1, 1)
pylab.plot(range(-nburn, nplot), mu1[:nburn+nplot], label='stage 1')
pylab.plot(range(-nburn, nplot), mu2[:nburn+nplot], label='stage 2')
pylab.ylabel('mean')
pylab.xlim(-nburn, nplot)
pylab.axvspan(-nburn, 0, facecolor='r', alpha=0.2)
pylab.legend()
pylab.subplot(2, 1, 2)
pylab.plot(range(-nburn, nplot), M[:nburn+nplot])
pylab.ylabel('transition point')
pylab.xlim(-nburn, nplot)
pylab.axvspan(-nburn, 0, facecolor='r', alpha=0.2)
pylab.xlabel('step')

# plot probability distribution in mu1-mu2 plane
fig = pylab.figure()
(Z, xedges, yedges) = pylab.histogram2d(mu1[nburn:], mu2[nburn:], bins=20,
        normed=True)
X = 0.5*(xedges[:-1]+xedges[1:])  # centers of histogram bins
Y = 0.5*(yedges[:-1]+yedges[1:])  # centers of histogram bins
(Y, X) = pylab.meshgrid(Y, X)
axis = fig.gca(projection='3d', azim=-50, elev=20)
surf = axis.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap=pylab.cm.jet)
axis.contour(X, Y, Z, zdir='z', offset=-pylab.amax(Z))
axis.contourf(X, Y, Z, 50, zdir='x', offset=min(mu1[nburn:]), colors='c')
axis.contourf(X, Y, Z, 50, zdir='y', offset=max(mu2[nburn:]), colors='c')
axis.set_xlabel('mu1')
axis.set_ylabel('mu2')
axis.set_zlabel('probability')
axis.set_zlim(-pylab.amax(Z), pylab.amax(Z))
axis.plot([mu1true], [mu2true], [-pylab.amax(Z)], 'r*')
fig.colorbar(surf)
pylab.show()
