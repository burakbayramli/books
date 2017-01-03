*Program 6.1
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)

source(noecho) a:\unit.src


do i = 1,12 ; @unit(lags=i) tb1yr ; end do i
@unit tb1yr
@unit(graph) tb1yr / corrs
do det = 1,3 ; @unit(det=det,lags=7) tb1yr ; end do det

dofor j = tb1yr tb3mo ; @unit(lags=7) j ; end dofor

@unit(lags=7,graph) tb1yr 1980:1 * corrs

dif tb1yr / drl
lin drl 1980:1 * ; # constant tb1yr{1} drl{1 to 7}


source(noecho) a:\laglength.src
@laglength(maxlag=8) tb1yr

source(noecho) a:\transform.src

@transform rgdp
table /
source(noecho) a:\transform_user.src
@transform_user rgdp
table
end =
*********
* Program 6.2
cal 1959 1 4
all 2001:1
open data a:\money_dem.xls
data(org=obs,format=xls)


source(noecho) a:\seasons.src
@seasons




