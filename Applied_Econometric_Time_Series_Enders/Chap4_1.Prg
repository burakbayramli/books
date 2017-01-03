*PROGRAM 4.1
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlrgdp = log(rgdp) - log(rgdp{1})
dif tb3mo / drs
set price = gdp/rgdp
set lrm2 = log(m2/price)  	;* Creates the log of the 'real' value of m2
dif lrm2 / dlrm2		;* Creates the first-difference of lrm2

lin(noprint) dlrgdp 14 * ; # constant
com aic_min = %nobs*log(%rss) + 2*%nreg, p_opt = 0
do p = 1,12
	lin(noprint) dlrgdp 14 *
	# constant dlrgdp{1 to p}
	compute aic = %nobs*log(%rss) + 2*(%nreg)
	if aic < aic_min {
		com p_opt = p
		com aic_min = aic
		}
end do p
if p_opt == 0 {
		dis 'An AR model is inappropriate. The autocorrelations are'
		cor(number=12,span=4,qstats) dlrgdp
		}
	else {
		lin dlrgdp / resids
		# constant dlrgdp{1 to p_opt}
compute aic = %nobs*log(%rss) + 2*(%nreg)
		dis 'The aic with' p_opt 'lags is' aic
	}
end if

*Jazzing up the Program
com max_lag = 16, diffs = 1
dofor x = dlrgdp dlrm2 drs

lin(noprint) x max_lag+diffs+1 * ; # constant
com aic_min = %nobs*log(%rss) + 2*%nreg, p_opt = 0
  do p = 1,max_lag
	lin(noprint) x max_lag+diffs+1 *
	# constant x{1 to p}
	compute aic = %nobs*log(%rss) + 2*(%nreg)
	if aic < aic_min {
		com p_opt = p
		com aic_min = aic
		}
dis aic aic_min p p_opt
end do p
if p_opt == 0 {
		dis 'An AR model is inappropriate. The autocorrelations are'
		cor(number=12,span=4,qstats) x
		}
	else {
		lin x / resids
		# constant x{1 to p_opt}
		compute aic = %nobs*log(%rss) + 2*(%nreg)
		dis 'The aic with' p_opt 'lags is' aic
	}
End DOFOR
end =
*************
*Program 4.2
* Benchmark
all 500
set dummy = 0

do i = 1,10000
do t = 1,500
	if t.ge. 250; com dummy(t) = 1
end do t
dis i
end do i
* 1 minute 18 seconds

do i = 1,10000
set dummy = %if(t.ge.250,1,0)
dis i
end do i

* 12 seconds

end =
**************

*Program 4.3  The threshold model
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlrgdp = log(rgdp) - log(rgdp{1})
set plus = %if(dlrgdp{1}>= 0,1,0)
pri 1 10 dlrgdp plus
set minus = 1 - plus

set y1_plus = plus*dlrgdp{1}
set y2_plus = plus*dlrgdp{2}
set y1_minus = minus*dlrgdp{1}
set y2_minus = minus*dlrgdp{2}

lin dlrgdp
# plus y1_plus y2_plus minus y1_minus y2_minus

compute low = 1959:2 + fix(.15*%nobs) , high = 2001:1 - fix(.15*%nobs)
compute rss_test = 1000000.0
set rss = 0.
set thresh_test = dlrgdp
order thresh_test
compute thresh = thresh_test(low)
do i = low,high
   set plus = %if(dlrgdp{1}<thresh_test(i),0,1)
   set minus = 1 - plus
   set y1_plus = plus*dlrgdp{1}
   set y2_plus = plus*dlrgdp{2}
   set y1_minus = minus* dlrgdp{1}
   set y2_minus = minus* dlrgdp{2}

   lin(noprint) dlrgdp
   # plus y1_plus y2_plus minus y1_minus y2_minus

   com rss(i) = %rss
   if %rss < rss_test {
   	compute rss_test = %rss
   	compute thresh = thresh_test(i)
   }
end do i
dis 'We have found the attractor'
dis ' Threshold = ' thresh
   set plus = %if(dlrgdp{1}<thresh,0,1)
   set minus = 1 - plus
   set y1_plus = plus*dlrgdp{1}
   set y2_plus = plus*dlrgdp{2}
   set y1_minus = minus* dlrgdp{1}
   set y2_minus = minus* dlrgdp{2}

   lin dlrgdp
   # plus y1_plus y2_plus minus y1_minus y2_minus

sca(Header='Residual Sums of Squares',style=lines,hlabel='Threshold') 1 ;
#  thresh_test rss low high

end =
*********
*Program 4.4
all 100
set y = 0.
set beta 1 1000 = 0.
do i = 1,1000
:redraw
	set eps = %ran(1)
	cor(number=4,qstats,noprint) eps
	if %signif < 0.001 ; branch redraw
	set y 2 * = y{1} + eps
	lin(noprint) y ; # constant y{1}
	com beta(i) = %beta(2)
end do
table / beta
end =
*******
**Program 4.5
all 6
seed 2001
set sum = 0                	;* The number of successes will be stored in sum
do j = 1,1000               	;* There are 1000 trials of the experiment
compute coin = fix(%if(%uniform(-1,1) > 0,2,1))
com x = %uniform(0,1)
com tet = fix(%uniform(1.0,5.0))
compute sum(coin+tet) = sum(coin+tet) + 1   ;*  add 1 to sum(total # faces)
end do j
print / sum

end =

****************
* PROGRAM 4.6
all 100
set discrep 1 1000 = 0.
set y = 0.
com alpha1 = 0.
dofor alpha1 =  .2 .5 .9 .99 0.
seed 2001
   do i = 1,1000
   	set y 2 * = alpha1*y{1} + %ran(1)
   	lin(noprint) y ; # constant y{1}
   	com discrep(i) =  alpha1 - %beta(2)
   end i
table / discrep
end dofor
end =
***********
*************
*PROGRAM 4.7
all 100
set tstat 1 10000 = 0.
set y = 0.0 ; compute y(1) = %ran(1)

do i = 1,10000
    set y 2 100 =  y{1} + %ran(1)
    diff y / dy
    linreg(noprint) dy * 100
    # constant y{1}
    compute tstat(i) = %tstats(2)
end do i

statistics(fractiles) tstat

end =
***********
* PROGRAM 4.8
all 200
seed 237

com alpha1 = 0.8
dofor alpha1 = 0.80 0.90 0.95 0.99
com rho = alpha1 - 1.0

compute hits10 = hits5 = hits1 = 0
set y = %ran(1)

*infobox(action=define,progress,lower=1,upper=10000) 'Replications Completed'
do j = 1,10000
*infobox(current=j)
      set y 2 * = alpha1*y{1} + sqrt(1-alpha1**2)*%ran(1)
   dif y / dy
   linreg(noprint) dy 102 *
   # constant y{1}
   compute df = %tstats(2)

   if df < -2.58 ; compute hits10 = hits10 + 1
   if df < -2.89 ; compute hits5 = hits5 + 1
   if df < -3.51 ; compute hits1 = hits1 + 1

end do j
*infobox(action=remove)
display ' rho = ' rho
display '    10%     5%     1%    '
display ###### hits10 ######  hits5  ######  hits1
display '     '

end dofor

end =




