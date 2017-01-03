*PROGRAM 4.9
all 200
seed 2001
set f 1 50000 = 0.
set y = 0.

*infobox(action=define,progress,lower=1,upper=50000) 'Replications Completed'
do j = 1,50000

*infobox(current=j)
:reset

*  TASK 1: COMPUTE the y series
set y 2 200 = y{1} + %ran(1)
diff y 2 200 dy
* TASK 2: Estimate the TAR Model
set flag = %if(y{1}<0,0,1)
set zplus = flag*y{1}
set zminus = (1-flag)*y{1}

linreg(noprint) dy 102 200
# zplus zminus

*  TASK 3
compute t1 = %tstats(1), t2 = %tstats(2)
if abs(t1*t2) < 0.0000001
      branch reset
* TASK 4: Calculate the Sample F-statistic
exclude(noprint)
# zplus zminus
compute f(j) = %cdstat
end do j
*infobox(action=remove)

*TASK 5
statistics(fractiles) f
end =

**********
* Program 4.10
allocate 150
seed 2002
set y = 0.0
set x = 0.
com beta1 = 1.0 , alpha1 = 0.1 , alpha2 = 0.1

*equation(noconstant,more,coeffs=|| 1.-alpha1, alpha1*beta1 ||) eq1 y 1 ; # x{1}
*equation(noconstant, more,coeffs=|| 1.-alpha2*beta1, alpha2 ||) eq2 x 1 ; # y{1}

equation(noconstant,more,coeffs=|| 1.-alpha1, alpha1*beta1 ||) eq1 y ; # y{1} x{1}
equation(noconstant, more,coeffs=|| 1.-alpha2*beta1, alpha2 ||) eq2 x ; # x{1} y{1}

group unit eq1 eq2
com [symmetric]v = || 1.0 , 0.0 | 0.0 , 1.0 ||
com success = 0.
set betahat 1 2000 = 0.

do i = 1,2000
sim(model=unit,results=sims) * 149 2 v
lin(noprint) sims(1) 51 * ; # constant sims(2)
com betahat(i) = %beta(2)


if beta1.gt.%beta(2)-1.96*%stderrs(2).and. beta1.le.%beta(2)+1.96*%stderrs(2)
	com success = success+1
dis beta1-1.96*%stderrs(2)    beta1+1.96*%stderrs(2)  success
end do i

sta(fractiles) betahat


dis 'The percentage of successes is:' success/20.
end =

*******
* Program 4.11 Antithetic Variables

all 50
seed 2001
set x = %uniform(5,15)
set alpha_hat 1 1000 = 0.
set beta_hat 1 1000 = 0.

NONLIN alpha beta
FRML monte y = beta*x**alpha
COM alpha = 0.48, beta = 0.98

do i = 1,1000
  set y = x**0.5 + %ran(1)
  NLLS(frml=monte,noprint) y
  com alpha_hat(i) = alpha, beta_hat(i) = beta
end do i

table / alpha_hat beta_hat
table * 500 alpha_hat beta_hat
table 501 * alpha_hat beta_hat

set alpha_bar 1 500 = 0.
set beta_bar 1 500 = 0.
NONLIN alpha beta
FRML monte y = beta*x**alpha
COM alpha = 0.48, beta = 0.98

do i = 1,500
   set eps = %ran(1)
   do j = 0,1
      set y = x**0.5 + (1-j)*eps - j*eps
*     nlls(frml=monte,noprint,method=simplex,iterations=5) y
      NLLS(frml=monte,noprint) y
      com alpha_bar(i) = alpha_bar(i) + 0.5*alpha
      com beta_bar(i) = beta_bar(i) + 0.5*beta
   end do j
end do i

tab /  alpha_bar beta_bar
end =
********
** Program 4.12  Intro to the Bootstrap
all 10
seed 2002
set x = 2+fix(10*%ran(2))/10.
set mean 1 100 = 0.
do i = 1,100
  set y = x(fix(%uniform(1,11)))
  sta(noprint) y ; com mean(i) = %mean
end do i
tab / mean

end =
*****
* Program 4.13 Bootstrap Regression Coefficients
all 50
seed 2001
set x = %uniform(5,15)
set y = x**0.5 + %ran(1)

NONLIN alpha beta
FRML monte y = beta*x**alpha
COM alpha = 0.48, beta = 0.98
NLLS(frml=monte) y / e
com alpha_hat = %beta(1) , beta_hat = %beta(2)

set alpha_star 1 1000 = 0.
set beta_star 1 1000 = 0.

do i = 1,1000
  set e_star = e(fix(%uniform(1,51)))
  set y_star = beta_hat*x**alpha_hat + e_star

  NONLIN alpha beta
  FRML monte y_star = beta*x**alpha
  COM alpha = alpha_hat, beta = beta_hat
  NLLS(frml=monte,noprint) y_star
  com alpha_star(i) = %beta(1) , beta_star(i) = %beta(2)
end do i
sta(fractiles) alpha_star
sta(fractiles) beta_star
end =

*****
* Program 4.14 Bootstrap GDP
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls) /
set dlrgdp = log(rgdp) - log(rgdp{1})
seed 2001

lin dlrgdp  / resids
# dlrgdp{1 to 2} constant

com beta1_hat = %beta(1), beta2_hat = %beta(2) , beta0_hat = %beta(3)
com se_2 = %STDERRS(2), t2 = %tstats(2)


dis 'Normal Approximation'
com l = beta2_hat - 1.644*se_2 , u = beta2_hat + 1.644*se_2
dis '  90% '  l u
com l = beta2_hat - 1.96*se_2 ,  u = beta2_hat + 1.96*se_2
dis '  95% '  l u
com l = beta2_hat - 2.57*se_2 ,  u = beta2_hat + 2.57*se_2
dis '  99% '  l u

sta resids

com boot_num = 1000
set beta2_star 1 boot_num = 0
set y_star = 0.
* Bootstrap the residuals
do k = 1,boot_num
	com ii = fix(%uniform(1959:2,2001:1))
	com y_star(1) = dlrgdp(ii), y_star(2) = dlrgdp(ii+1)

        set e_star = resids(fix(%uniform(1959:4,2001:1+1)))
        set y_star 3 * = beta1_hat*y_star{1} + beta2_hat*y_star{2} + beta0_hat + e_star
	lin(noprint) y_star ; # y_star{1 to 2} constant

        compute beta2_star(k) = %beta(2)
end do k

dis 'Percentile '
sta(fractiles) beta2_star

order beta2_star
dis 'Confidence intervals for beta2'
dis ' 10% ' beta2_star(fix(.05*boot_num))  beta2_star(fix(.95*boot_num))
dis '  5% ' beta2_star(fix(.025*boot_num)) beta2_star(fix(.975*boot_num))
dis '  1% ' beta2_star(fix(.005*boot_num)) beta2_star(fix(.995*boot_num))

end =

***********
*Program 4.14a
cal 1959 1 4
all 2001:1
open data c:\rats2\money_dem.xls
data(org=obs,format=xls) /
set dlrgdp = log(rgdp) - log(rgdp{1})
seed 2001

lin dlrgdp  / resids
# dlrgdp{1 to 2} constant

com beta1_hat = %beta(1), beta2_hat = %beta(2) , beta0_hat = %beta(3)
com se_2 = %STDERRS(2), t2 = %tstats(2)


dis 'Normal Approximation'
com l = beta2_hat - 1.644*se_2 , u = beta2_hat + 1.644*se_2
dis '  90% '  l u
com l = beta2_hat - 1.96*se_2 ,  u = beta2_hat + 1.96*se_2
dis '  95% '  l u
com l = beta2_hat - 2.57*se_2 ,  u = beta2_hat + 2.57*se_2
dis '  99% '  l u

sta resids

com boot_num = 1000
set beta2_star 1 boot_num = 0
set boott_2 1 boot_num = 0.
set y_star = 0.

* Bootstrap the residuals
do k = 1,boot_num
	com ii = fix(%uniform(1959:2,2001:1))
	com y_star(1) = dlrgdp(ii), y_star(2) = dlrgdp(ii+1)

	set e_star = resids(fix(%uniform(1959:4,2001:1+1)))
        set y_star 3 * = beta1_hat*y_star{1} + beta2_hat*y_star{2} + beta0_hat + e_star
	lin(noprint) y_star ; # y_star{1 to 2} constant

        compute beta2_star(k) = %beta(2)
        compute boott_2(k) = (%beta(2)-beta2_hat)/%STDERRS(2)

end do k

*****

dis 'Percentile '
sta(fractiles) beta2_star

order beta2_star
dis 'Confidence intervals for beta2'
dis ' 10% ' beta2_star(fix(.05*boot_num))  beta2_star(fix(.95*boot_num))
dis '  5% ' beta2_star(fix(.025*boot_num)) beta2_star(fix(.975*boot_num))
dis '  1% ' beta2_star(fix(.005*boot_num)) beta2_star(fix(.995*boot_num))


dis 'Bootstrap T'
order boott_2
dis 'Confidence intervals for beta2'
com l = beta2_hat - (se_2*boott_2(fix(.95*boot_num))) , u = beta2_hat - (se_2*boott_2(fix(.05*boot_num)) )
dis ' 10% '  l u
com l = beta2_hat - (se_2*boott_2(fix(.975*boot_num))) , u = beta2_hat - (se_2*boott_2(fix(.025*boot_num)) )
dis '  5% '  l u
com l = beta2_hat - (se_2*boott_2(fix(.995*boot_num))) , u = beta2_hat - (se_2*boott_2(fix(.005*boot_num)) )
dis '  1% '  l u

end =













