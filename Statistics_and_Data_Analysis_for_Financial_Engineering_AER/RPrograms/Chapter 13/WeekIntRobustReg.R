dat = read.table(file="WeekInt.txt",header=T)

attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
year2 = year + (month-1)/12 + (day-1)/253
n = length(year2)
year2 = year2[2:n]