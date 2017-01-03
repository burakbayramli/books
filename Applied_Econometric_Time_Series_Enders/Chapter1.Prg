*Program 1.1
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

table

set dlrgdp = log(rgdp) - log(rgdp{1})
set dlm2 = log(m2) - log(m2{1})
set dlm3 = log(m3) - log(m3{1})
dif tb3mo / drs
dif tb1yr / drl
set price = gdp/rgdp
set dlp = log(price) - log(price{1})

spgraph(hea='Graphs of Five Principal Series',hfi=2,vfi=2)
gra(hea='Panel 1: Time path of dlm2 and dlm3',key=upleft,patterns) 2 ; # dlm2 ; # dlm3
gra(hea='Panel 2: Time path of dlrgdp') 1 ; # dlrgdp
gra(hea='Panel 3: Time path of drs') 1 ; # drs
gra(hea='Panel 4: Time path of dlp') 1 ; # dlp
spgraph(done)

lin drs / resids
# constant drs{1 to 7}
cor(number=12,partial=partial,qstats,span=4) resids

exclude
# drs{5 to 7}

exclude
# constant drs{5 to 7}

summarize
# drs{5 to 7}

test
# 6 7 8
# 0.1 0.1 0.1

test
# 1 6 7 8
# 0. 0.1 0.1 0.1

restrict(create) 3 resids
# 2 3
# 1 1 0
# 4 5
# 1 1 0
# 6 7
# 1 1 0

lin(robusterrors) drs
# constant drs{1 to 7}

gra(header='3-month and 1-yr T-bill rates',vlabel='annual percentage rate',patterns,key=upleft) 2
# tb1yr
# tb3mo

lin tb1yr / resids
# constant tb3mo

cor(number=12,partial=partial) resids / cors
gra(nodates,sty=5,key=7,header='ACF and PACF',patterns,number=0) 2 ; # cors ; # partial

dif resids / dresids
do i = 1,8
lin(noprint) dresids 1961:4 *
# resids{1} dresids{1 to i}

compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'Lags: ' i 'T-stat' %tstats(1) 'The aic = ' aic ' and sbc = ' sbc
end do i

end =
******************
* Program 1.2
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

log rgdp / lrgdp
sqrt rgdp / rgdp_rt
gra(overlay=line,header='Linearized RGDP?',key=below,patterns) 2
# lrgdp; # rgdp_rt

set time = t ;set t2 = t*t

lin(noprint) rgdp ;# constant time t2
compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc

nonlin a0 a1
frml model_2 rgdp = exp(a0 + a1*time)
com a0 = 1., a1 = .1
nlls(frml=model_2) rgdp

compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc

nonlin a0 a1
frml model_3 rgdp = (a0 + a1*time)**2
com a0 = %beta(1), a1 = %beta(2)
nlls(frml=model_3) rgdp

compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc
end =
***************
*Program 1.3

all 100
set y = -5 + 0.1*t
set theta1 = (1 + exp(-.5*y))**-1
set theta2 = (1 + exp(-y))**-1
set theta3 = (1 + exp(-2*y))**-1
set theta4 = (1 + exp(-5*y))**-1
com labels = || ' 0.5' , '1.0', '2.0', '5.0,' ||

sca(header='Effects of Gamma on Theta',style=lines,patterns,klabels=labels,key=below, $
vlabel='THETA',hlabel='GAMMA') 4
# y theta1
# y theta2
# y theta3
# y theta4

end =
***************
* Program 1.4
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlm3 = log(m3) - log(m3{1})
lin dlm3 ; # constant dlm3{1}
compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc

nonlin a0 a1  b0 b1 gamma    gamma.ge.0.
frml lstar dlm3 = a0 + a1*dlm3{1}  + ( b0 + b1*dlm3{1})/(1+exp(-gamma*(dlm3{1})))
lin(noprint) dlm3 ; # constant dlm3{1}
com a0 = %beta(1), a1 = %beta(2), b0 = 1. , b1 = 1., gamma = 500.
nlls(frml=lstar) dlm3
dis %converged

compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc
dis %converged

nonlin  a1  b0 b1 gamma    gamma.ge.0.
frml lstar dlm3 =  a1*dlm3{1}  + ( b0 + b1*dlm3{1})/(1+exp(-gamma*(dlm3{1})))
lin(noprint) dlm3 ; # constant dlm3{1}
com a0 = %beta(1), a1 = %beta(2), b0 = 1. , b1 = 1., gamma = 500.
nlls(frml=lstar) dlm3  / resids

compute aic = %nobs*log(%rss) + 2*(%nreg)
compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
dis 'The aic = ' aic ' and sbc = ' sbc

* Calculate the range of the autoregressive coefficient
dis a1+b1/(1+exp(-gamma*0.085577))
dis a1+b1/(1+exp(-gamma*-0.00538))

******** Maximum likelihood

nonlin a0 a1  b0 b1 gamma  var gamma.ge.0.
frml expression = a0 + a1*dlm3{1} + ( b0 + b1*dlm3{1})/(1+exp(-gamma*(dlm3{1})))
frml lstar = -log(var) - (dlm3 - expression)**2/var
lin(noprint) dlm3 ; # constant dlm3{1}
com a0 = %beta(1), a1 = %beta(2), b0 = 1. , b1 = 1., gamma = 500. , var = %seesq

maximize(iters=20,method=simplex) lstar
maximize(iters=200) lstar

************
* ARMA Model
dif tb3mo / drs

set temp = 0.
nonlin  a1  a7 b  var
frml e = drs - a1*drs{1} - a7*drs{7} - b*temp{1}
frml L = (temp=e), -log(var) - (e)**2/var
com a1 = .4, a7 = -.3, b = .5, var = 1.
max L  9 *
*  The Higher-order ARMA Process
set temp = 0.
nonlin  a1  a7 b1 b2  var
frml e = drs - a1*drs{1} - a7*drs{7} - b1*temp{1} - b2*temp{2}
frml L = (temp=e), -log(var) - (e)**2/var
com a1 = 0.4, a7 = -.3, b1 = .5, b2 = 0.3, var = 1.
max L  9 *

**************
* Bilinear Model
set temp = 0.
nonlin  a1 b1 c1 var
frml e = dlm3 - a1*dlm3{1} - b1*temp{1} - c1*dlm3{1}*temp{1}
frml L = (temp=e), -log(var) - (e)**2/var
com a1 = .8,  b1 = 0., c1 = 0.1, var = 0.01
max(method=simplex,iters=4) L  3 *
max L  3 *

set temp = 0.
nonlin  a1 b1 var
frml e = dlm3 - a1*dlm3{1} - b1*temp{1}
frml L = (temp=e), -log(var) - (e)**2/var
com a1 = .8,  b1 = 0., var = 0.01
max(method=simplex,iters=4) L  3 *
max L  3 *

************
* ARCH model of the spread
set spread = tb1yr - tb3mo
lin spread / resids; # constant spread{1 to 3}
cor(number=12,span=4,qstats) resids

set r2 = resids**2
lin r2 ; # constant r2{1 to 3}
compute trsq = %nobs*%rsquared
cdf chisqr trsq 3

nonlin  a0 a1 a2 a3 b0 b1 b2 b3 b0.ge.0. b1.ge.0. b2.ge.0. b3.ge.0.
frml e = spread - a0 - a1*spread{1} - a2*spread{2} - a3*spread{3}
frml h = b0 + b1*e{1}**2 + b2*e{2}**2 + b3*e{3}**2
frml L =  -log(h) - (e)**2/h
lin(noprint) spread ; # constant spread{1 to 3}
com a0 = %beta(1), a1 = %beta(2) , a2 = %beta(3), a3 = %beta(4) , b0 = %seesq, b1 = 0.2,  b2 = 0.2, b3 = 0.2
max(method=simplex,iters=4) L
max(iters=200) L

*** IGARCH
set temp = 0.
nonlin  a0 a1 a2 a3 b0 b1 c1 b0.ge.0. b1.ge.0. c1.ge.0.
frml e = spread - a0 - a1*spread{1} - a2*spread{2} - a3*spread{3}
frml h = b0 + b1*e{1}**2 + c1*temp{1}
frml L = (temp = h), -log(h) - (e)**2/h
lin(noprint) spread ; # constant spread{1 to 3}
com a0 = %beta(1), a1 = %beta(2) , a2 = %beta(3), a3 = %beta(4) , b0 = %seesq, b1 = 0.2,  c1 = 0.5
max(method=simplex,iters=5) L 7 *
max(iters=200) L  7 *

*Replace NONLIN above with the following
* nonlin  a0 a1 a2 a3 b0 b1 c1 b0.ge.0. b1.ge.0. c1.ge.0. b1+c1.eq.1.

***
set temp1 =0.  ; set temp2 = 0.
nonlin  a0 a1 beta b0 b1 c1 b0.ge.0. b1.ge.0. c1.ge.0.  b1+c1.eq.1.
frml e = spread - a0 - a1*spread{1} - beta*temp1{1}
frml h = b0 + b1*e{1}**2 + c1*temp2{1}
frml L = (temp1 = e), (temp2 = h), -log(h) - (e)**2/h

box(constant,ar=1,ma=1,noprint) spread
com a0 = %beta(1), a1 = %beta(2) , beta = %beta(3),  b0 = %seesq, b1 = 0.2,  c1 = 0.5
max(method=simplex,iters=5) L 7 *
max(iters=200) L  7 *

end =



