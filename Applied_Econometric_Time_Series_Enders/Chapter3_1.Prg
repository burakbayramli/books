***** Program 3.1
all 10
scratch 1
set 1 = 5
pri / 1

end =

***** Program 3.2
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

pri 59:1 59:4 rgdp
pri(nodates) / rgdp
pri 1 4 rgdp
pri(nodates) 1 4 rgdp
print(nodates) 1 4 tb1yr

print(nodates) 165 169 tb1yr

dis tb1yr(2001:1)  tb1yr(169)

pri 2001:1-8 * rgdp
set dlrgdp = log(rgdp) - log(rgdp{1})
lin dlrgdp * 100
# constant dlrgdp{1}

lin dlrgdp 2001:1-99 *  resids
# constant dlrgdp{1}

lin dlrgdp 69 * resids
# constant dlrgdp{1}

dis %DATELABEL(2001:1-100)

dis %cal(1976,1)
table
pri 1 4 3 8

end =
*************
***** Program 3.3
cal 1959 1 4
all 4 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)
tab

eqv 1 to 4
resids1 resids2 resids3 resids4
tab / 1 to 4
scratch 2 / b
tab / b to b+2

dis %l(7)
dis %l(rgdp)
do i = 1,5
   dis i
end do i

set dlrgdp = log(rgdp) - log(rgdp{1})

do p = 1,12
	lin dlrgdp
	# constant dlrgdp{1 to p}
end do p

do p = 1,12
		lin(noprint) dlrgdp 14 *
		# constant dlrgdp{1 to p}
		compute aic = %nobs*log(%rss) + 2*(%nreg)
		compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
		dis 'Lags:' p 'AIC =' aic 'SBC ='  sbc 't =' %tstats(p+1)
end do p

dif tb1yr / drl
do p = 0,4
  do q = 0,4
	box(ar=p,ma=q,noprint) drl 1960:4 *
	compute aic = %nobs*log(%rss) + 2*(%nreg)
	compute sbc = %nobs*log(%rss) + (%nreg)*log(%nobs)
	dis  'Order ' p q 'The aic = ' aic ' and sbc = ' sbc
end do q
end do p

box(ma=2) drl

end =
**************

