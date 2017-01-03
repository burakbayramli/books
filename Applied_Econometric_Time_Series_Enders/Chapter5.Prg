* Program 5.1
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls) /

log rgdp / lrgdp
log m2 / lm2
set lp = log(gdp/rgdp)
tab

lin lm2
# constant lrgdp lp tb3mo

lin lm2
# 0 8 10 6       ;* NOTE: 0 is equivalent to constant. If you type: pri / 0 you get constant

com a = || 0, 8, 10 , 6 ||
lin lm2
# a

dofor i = tb3mo tb1yr
		com a = || 0, 8, 10, i ||
		lin lm2
		# a
end do for

com names = || 'rgdp', 'tb3mo' ||
dis names(1,2)
pri / names

com b = || rgdp, tb3mo ||
pri / b
pri / b(1,2)
dis b(1,2)(2)

com [vector[string]]c = || 'rgdp', 'tb3mo' ||
dis c
dis c(2)

com d = || 1.0, 2 | 3 , 4|| , e = || 1, 2 ||
dis d e

com d = || 1.0, 2 | 3 , 4|| , [vect[integer]] e = || 1, 2 ||
dis d e
com [vect] x = || 1.0, 4.0, -3.9 , 2.0  ||
com y = || 1.0, 4.0, -3.9 , 2.0  ||
dis x(2)  y(1,2)

com i11 = 1 ,i12 = 2 , i13 = 3 , i14 = 4
com [rect[integer]] inum = ||i11, i12 | i13, i14||
dis inum

com j11 = 1 ,j12 = 2 , j13 = 3.9 , j14 = 4
com [rect[integer]] inum = ||j11, j12 | j13, j14||
end =
************

***Program 5.2
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
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
estimate(sigma,outsigma=v,residuals=resids,coeffs=ca)
dis ca
dis ca(37,2)

pri * 16 resids
pri * 16 resids(2)
dis resids(2)(16)

dis v
dis %decomp(v)

com g = tr(%decomp(v))
dis '  ' ; dis 'Decomposed Matrix' g

do i = 1,3
cor(qstats,span=4) resids(i)
end do


dec vector[integer] reglist
compute reglist=||constant ||

dofor i = dlrgdp dlrm2 drs
enter(varying) reglist
# reglist i{1 to 12}
lin dlrgdp
# reglist
end dofor i

sea seasons

dec vector[integer] temp_det perm_det
com temp_det = ||constant||
com perm_det = ||constant ||
com print = 0  , aic_min = 100000000.
do i = 1,3
	if i == 2 {
	enter(varying) temp_det
	# perm_det seasons{1 to 3}
        }
	if i == 3 ; com print = 1
	system 1 to 3
	vars dlrgdp dlrm2 drs
	lags 1 to 12
	det temp_det
	end(system)
	estimate(print=print)
	com aic = %nobs*%logdet + 2*(%eqnsize(1))*3; dis aic

  if aic < aic_min {
	  enter(varying) perm_det
	  # temp_det
	  com aic_min = aic
	  }
	  enter(varying) temp_det
	  # perm_det
end do
*****************
** The Near VAR

set dlm3 = log(m3) - log(m3{1})
set price = gdp/rgdp
set dlp = log(price) - log(price{1})

dec vector[integer] templist reglist

*dofor j = dlrgdp dlm3 drs dlp

compute templist=||constant||
compute reglist=||constant||

dofor i = dlrgdp dlm3 drs dlp
enter(varying) templist
# reglist i{1 to 4}
	lin(noprint) dlm3
	# templist
	* lin(noprint) j  ;* Use in the
        * templist        ;* modifoed program

	exclude(noprint)
	# i{1 to 4}
	if %signif < .05 {
	enter(varying) reglist
	# reglist i{1 to 4}
	}
enter(varying) templist
# reglist
end dofor i
lin dlm3
# reglist
* lin j     ;* These 3 lines are used
* reglist   ;* when looping over all j
* end do j

end =

***************
* PROGRAM 5.3 Moving average representation
compute number = 24
all number

com alpha = ||1.1 , -.4 , .2  ||
com beta = ||.7 , -.3||

COMPUTE p = %cols(alpha)
COMPUTE q = %cols(beta)

dec vect a(number) b(number)
com a = %const(0.) , b = %const(0.)
ewise a(i) = %if(i<=p, alpha(1,i), 0.0)
ewise b(i) = %if(i<=q ,beta(1,i), 0.0)

dec vect phi(number)
com phi(1) = 1.

 do j = 1,number-1
 com phi(j+1) = b(j)
 do k = 1,j  ; com phi(j+1) = phi(j+1) + phi(j+1-k)*a(k) ; end do k
 end do j
set resp = phi(t)
gra(style=bar,Header='Impulse Responses') 1 ; # resp
end =
***************
*Program5.4
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls) /

dif tb3mo / drs
com number = 24

com ar = ||2,7||
com ma = ||1,3||
box(constant,ar=ar,ma=ma) drs

COMPUTE p = %cols(ar)
COMPUTE q = %cols(ma)
com flag = %rows(%beta) - p -q

dec vect a(number) b(number)
com a = %const(0.) , b = %const(0.)

do i = 1,p ; com a(ar(1,i)) = %beta(i+flag) ; end do i
do i = 1,q ; com b(ma(1,i)) = %beta(i+p+flag) ; end do i

dec vect phi(number)
com phi(1) = 1.

 do j = 1,number-1
 com phi(j+1) = b(j)
 do k = 1,j  ; com phi(j+1) = phi(j+1) + phi(j+1-k)*a(k) ; end do k
 end do j
set resp = phi(t)
gra(nodates,style=bar,Header='Impulse Responses') 1 ; # resp

end =
*************
* Program 5.5
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls) /

*table
log rgdp / lrgdp
dif lrgdp / dlrgdp
lin(define=eq1) dlrgdp / resids
# constant dlrgdp{1 to 2}

make(lastreg) x  4 2001:1
dis x
dis x(2,3)

make y 4 2001:1
# dlrgdp
dis y
dis y(3,1)

compute xx = tr(x)*x
com xx_inv = inv(xx)
com xy = tr(x)*y
com beta = xx_inv*xy
dis beta

dis inv(tr(x)*x)*tr(x)*y

com P = x*xx_inv*tr(x)
com test = p*p - p ; dis test
dis %trace(p)

com y_hat = p*y  ; dis y_hat
com e = y - y_hat ; dis e
pri / resids

com m = %identity(166) - p
dis %identity(166)
dis m*p
com test = m*m - m ; dis test

com v = %scalar(tr(e)*e)/163 ; dis v
dis %sqrt(v)
com v_beta = %scalar(v)*xx_inv
dis v_beta
dis %sqrt(v_beta(1,1)) %sqrt(v_beta(2,2)) %sqrt(v_beta(3,3))

com q = 1
com R = || 1. , 0. , 0. ||
com c = || 0.||
com f =  tr(c - R*beta)*inv( r*xx_inv*tr(r) )*(c - r*beta)
com f1 = %scalar(f)/(q*v) ; dis f1
dis %sqrt(f1)

com n = 2
com R = || 0. , 1. , 0. | 0., 0., 1. ||
com c = || 0. | 0.||
com f =  tr(c - R*beta)*inv( r*xx_inv*tr(r) )*(c - r*beta)
com f1 = %scalar(f)/(2*v) ; dis f1

exc ; # dlrgdp{1 2}

dec vector[series] errors(1)
set errors(1) = e(t,1)
pri 1 5 errors(1) resids
end =




