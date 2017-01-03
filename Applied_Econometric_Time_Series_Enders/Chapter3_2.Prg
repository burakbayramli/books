* Chapter 3 part 2
*Program 3.4

cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlrgdp = log(rgdp) - log(rgdp{1})
set price = gdp/rgdp
set lrm2 = log(m2/price)
dif lrm2 / dlrm2
dif tb3mo / drs

do lags = 16,8,-4
system(model=chap3)
vars dlrgdp dlrm2 drs
lags 1 to lags
det constant
end(system)
estimate(resids=unrestrict,noprint) 59:2+lags *
com aic_u = %nobs*%logdet + 2*(3*lags+1)*3
com sbc_u = %nobs*%logdet + log(%nobs)*(3*lags+1)*3

system(model=chap3)
vars dlrgdp dlrm2 drs
lags 1 to lags-4
det constant
end(system)
estimate(resids=restrict,noprint) 59:2+lags *
com aic_r = %nobs*%logdet + 2*(3*(lags-4)+1)*3
com sbc_r = %nobs*%logdet + log(%nobs)*(3*(lags-4)+1)*3

dis 'Lags = '  lags 'aic_r = ' aic_u 'sbc = ' sbc_u
dis 'Lags = '  lags-4 'aic = ' aic_r 'sbc = ' sbc_r

ratio(degrees=3*4*3,mcorr=3*lags+1,noprint) 59:2+lags *
# unrestrict
# restrict
dis 'Significance level = ' %signif   ; dis '  '
end do

set f_ar1 = 0.
set f_ar2 = 0.
do i = 100,168
	lin(noprint,define=ar1) dlrgdp 3 i; # constant dlrgdp{1}
	forecast 1 1
	# ar1 f_ar1
	lin(noprint,define=ar2) dlrgdp 3 i; # constant dlrgdp{1 to 2}
	forecast 1 1
	# ar2 f_ar2
end do i

set pe_1 101 169 = (dlrgdp - f_ar1)**2
set pe_2 101 169 = (dlrgdp - f_ar2)**2
sta(noprint)  pe_1
dis 'The MSPE from the AR(1) is: ' %mean
sta(noprint) pe_2
dis 'The MSPE from the AR(2) is: ' %mean

end =

**********
* Program 3.5
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls) /
scratch 6 / scr_no

do i = 1,6
	set scr_no+i  = log((i+1){0}) - log((i+1){1})
	labels scr_no+i ; # 'DL'+%l(i+1)
end do i

tab / scr_no+1 to scr_no+6
end =
***************
* Program 3.6
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlrgdp = log(rgdp) - log(rgdp{1})
set dlm3 = log(m3) - log(m3{1})
dif tb3mo / drs
set price = gdp/rgdp
set dlp = log(price) - log(price{1})

spgraph(hea='Graphs of Four Principal Series',hfi=2,vfi=2)
com j = 0
	dofor i = dlm3 dlrgdp drs dlp
	com j = j + 1
	com headers = 'Panel ' + j + ': Time Path of ' + %label([series]i)
	gra(hea= headers) 1 ; # i
end do i
spgraph(done)

end =
*********
* Program 3.7
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

scratch 3 / scr_no

dofor i = rgdp m2 tb3mo
	com j = j + 1
 	set scr_no+j = log(i{0}) - log(i{1})
	labels scr_no+j ; # 'DL'+%l(i)
end dofor
table / scr_no+1 scr_no+2 scr_no+3
com a = %s('rgdp') ; dis a

dofor i = rgdp m2 tb3mo
	set %s('dl'+%l(i)) = log(i{0}) - log(i{1})
end dofor
tab
end =
**************
*Program 3.8
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

set dlrgdp = log(rgdp) - log(rgdp{1})
set dlm3 = log(m3) - log(m3{1})

dofor i = 7 8 14
do p = 1,2
	lin(noprint,entries=i) dlrgdp 6 *
	# constant dlrgdp{1 to p} dlm3 dlm3{1 to 4}
	compute aic = %nobs*log(%rss) + 2*(%nreg)
	compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
	dis  'The aic = ' aic ' and sbc = ' sbc
end do p
end dofor i
lin dlrgdp 6 *
# constant dlrgdp{1 to 2} dlm3

dif tb3mo / drs
dif tb1yr / drl

com lags = 13, sign = .5
while sign > 0.05  {
	com lags = lags - 1
	lin(noprint) drl
	# constant drl{1 to lags}
        exclude(noprint) ; # drl{lags}
        com sign = %signif
        dis 'Significance of lag' lags  '= ' sign
        }
end while
lin drl
# constant drl{1 to lags}

*Jazzing up the program
set price = gdp/rgdp
set dlp = log(price) - log(price{1})
DOFOR i = dlrgdp dlm3 drs drl dlp

	com lags = 13, sign = .5
	while sign > 0.05  {
		com lags = lags - 1
		lin(noprint) i
		# constant i{1 to lags}
        	 	exclude(noprint) ; # [series]i{lags}
        		com sign = %signif
                		}
	dis %label([series]i) 'Lag length = ' lags
*	lin i ; # constant i{1 to lags}
END DOFOR

end =

