*****PROGRAM 2.1
cal 1959 1 4
all 2001:1
open data c:\money_dem.xls
data(org=obs,format=xls)

*table
log rgdp / lrgdp
dif lrgdp / dlrgdp
dif tb3mo / drs

set price = gdp/rgdp
set lrm2 = log(m2/price)  	;* Creates the log of the 'real' value of m2
dif lrm2 / dlrm2		;* Creates the first-difference of lrm2

system(model=chap2)
var dlrgdp dlrm2 drs
lags 1 to 12
det constant
end(system)
estimate(noprint,residuals=resids12)
compute aic =  %nobs*%logdet + 2*(37*3)
compute sbc =  %nobs*%logdet + 37*3*log(%nobs)
dis 'aic = '  aic 'sbc = ' sbc

system(model=chap2)
var dlrgdp dlrm2 drs
lags 1 to 8
det constant
end(system)
estimate(noprint,residuals=resids8) 1962:2 *
compute aic =  %nobs*%logdet + 2*(25*3)
compute sbc =  %nobs*%logdet + 25*3*log(%nobs)
dis 'aic = '  aic 'sbc = ' sbc

ratio(degrees=4*3*3,mcorr=37) 1962:2 *
# resids12
# resids8
**************
* Now do the Block Exogeneity Test
system(model=unrestricted)
var dlrgdp dlrm2
lags 1 to 12
det constant drs{1 to 12}
end(system)
estimate(noprint,residuals=unrest)

system(model=restricted)
var dlrgdp dlrm2
lags 1 to 12
det constant
end(system)
estimate(noprint,residuals=rest)

ratio(degrees=24,mcorr=37)
# unrest ; # rest

**************
system(model=chap2)
var dlrgdp dlrm2 drs
lags 1 to 12
det constant
end(system)
estimate(outsigma=v)

errors(impulses,model=chap2) 3 24 v


FORECAST(model=chap2,results=fores)  *  12  2001:2
pri / fores

*********
*** A near VAR with Seemingly Unrelated Regressions
lin(define=eq1) dlrgdp ; # constant dlrm2{1 to 12} drs{1 to 12}
lin(define=eq2) dlrm2 ; # constant dlrm2{1 to 12} drs{1 to 12}
lin(define=eq3) drs ; # constant dlrgdp{1 to 12} dlrm2{1 to 12} drs{1 to 12}

sur(outsigma=v) 3
# eq1 ; # eq2 ; # eq3

compute aic =  %nobs*%logdet + 2*(2*25+37)
compute sbc =  %nobs*%logdet + (2*25+37)*log(%nobs)
dis 'aic = '  aic 'sbc = ' sbc

group chap2_sur eq1 eq2 eq3

errors(impulses,model=chap2_sur) 3 24 v

lin(define=spread) tb1yr / resids
# constant tb3mo

system(model=term)
var tb1yr tb3mo
lags 1 to 6
det constant
ect spread
end(system)

estimate(outsigma=s,residuals=resid)

errors(impulses,model=term) 2 24 s

declare rectangular[series] impulses(2,2)
com implabels = || '1 year','3 month'||

impulse(model=term,result=impulses,noprint) * 24 * s
spgraph(hfields=2,vfi=1,header='Impulse Response Functions')
GRAPH(HEADER='Shocks to the One Year Rate',KEY=upright,NUMBER=0,patterns,klabels=implabels) 2
# impulses(1,1) ; # impulses(2,1)
GRAPH(HEADER='Shocks to the 3-Month Rate',KEY=upright,NUMBER=0,patterns,klabels=implabels) 2
   # impulses(1,2) ; # impulses(2,2)
spgraph(done)
***********
* Structural Decompositions

com g = || 1. , 0 | 0. , 1. ||
impulse(model=term,results=impulses,decomp=g) * 24 *

* Recall that the long run relationship is
lin(define=spread) tb1yr / resids
# constant tb3mo
com x = %beta(2)

com g = || 1. , x | 0. , 1. ||
impulse(model=term,results=impulses,decomp=g) * 24 *

* Sims-Bernanke
nonlin g21
dec frml[rect] g_form
frml g_form = || 1., 0. | g21 , 1. ||
com g21 = 0.01
cvmodel(factor=g) %sigma g_form
*cvmodel(factor=g,iters=4,method=simplex) %sigma g_form
*cvmodel(factor=g) %sigma g_form
dis g

dis %sigma

impulse(model=term,results=impulses,decomp=g) * 24 *
*********
* EXAMPLE 2
system(model=chap2)
var dlrgdp dlrm2 drs
lags 1 to 12
det constant
end(system)
estimate(outsigma=v,noprint)

errors(impulses,model=chap2) 3 24 v

nonlin g21 g23
dec frml[rect] g_form
frml g_form = || 1., 0. , 0. | g21, 1. , g23 | 0., 0. ,1. ||
com g21 = -.2 , g23 = 0.3
cvmodel(factor=g) %sigma g_form

impulse(model=chap2,decomp=g) * 24 *

declare rectangular[series] impulses(3,3)
impulse(model=chap2,result=impulses,decomp=g) * 24 *
 set r1 1 12 = impulses(1,3)/impulses(1,1)(1)
 set r2 1 12 = impulses(2,3)/ impulses(2,2)(1)
 set r3 1 12 = impulses(3,3)/impulses(3,3)(1)

com implabels = || 'dlrgdp','dlrm2', 'drs'||
GRAPH(HEADER='Responses to an Interest Rate Shock',KEY=upright,patterns,number=1,klabels=implabels,vlabel='standard deviations') 3
# r1 ; # r2 ;  # r3

* The Blanchard-Quah Example
**Use only 1-lag
system(model=chap2)
var dlrgdp dlrm2 drs
lags 1
det constant
end(system)
estimate(outsigma=v,noftests)

dis %varlagsums
COMPUTE C = %VARLAGSUMS
COMPUTE S1 = %MQFORM(%SIGMA,tr(INV(C)))
COMPUTE S2 = %DECOMP(S1)

dis s2

com g = C*S2 ; dis g

impulse(model=chap2,decomp=g) * 24 *
**************
**Now use 12 lags
system(model=chap1)
var dlrgdp dlrm2 drs
lags 1 to 12
det constant
end(system)
estimate(outsigma=v,noftests)
dis %varlagsums

compute g=%varlagsums*%decomp(%mqform(%sigma,tr(inv(%varlagsums))))

declare rectangular[series] impulses(3,3)
impulses(model=chap1,result=impulses,decomp=g) * 24 *
/* Not used in the text
 set r1 1 12 = impulses(1,1)/%sigma(1,1)**.5
 set r2 1 12 = impulses(2,1)/%sigma(2,2)**.5
 set r3 1 12 = impulses(3,1)/%sigma(3,3)**.5

com implabels = || 'dlrgdp','dlrm2', 'drs'||
GRAPH(HEADER='Responses to Supply Shock',KEY=upright,patterns,number=1,klabels=implabels) 3
# r1 ; # r2 ;  # r3

set r4 1 12 = impulses(1,2)/%sigma(1,1)**.5
set r5 1 12 = impulses(2,2)/%sigma(2,2)**.5
set r6 1 12 = impulses(3,2)/%sigma(3,3)**.5

com implabels = || 'dlrgdp','dlrm2', 'drs'||
GRAPH(HEADER='Responses to LM Shock',KEY=upright,patterns,number=1,klabels=implabels,vlabel='standard deviations') 3
# r4 ; # r5 ;  # r6
*/

 set r7 1 12 = impulses(1,3)/%sigma(1,1)**.5
 set r8 1 12 = impulses(2,3)/%sigma(2,2)**.5
 set r9 1 12 = impulses(3,3)/%sigma(3,3)**.5

com implabels = || 'dlrgdp','dlrm2', 'drs'||
GRAPH(HEADER='Responses to a Fiscal Shock',KEY=upright,number=1,klabels=implabels,patterns) 3
# r7 ; # r8 ;  # r9







