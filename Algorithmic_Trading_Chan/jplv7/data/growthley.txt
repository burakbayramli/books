% a data set from Fernandez, Ley and Steel
% on 72 countries and their growth rates over 1960-80 period

load growthley.dat;
% 1 longitude
% 2 latitude
% 3 gdp growth rate 1960-1980
% 4 PrSc Enroll
% 5 Life Exp
% 6 GDPsh560
% 7 Mining
% 8 Eco Org
% 9 Yrs Open
% 10 English %
% 11 Foreign %
% 12 R FEX Dist
% 13 Equip Inv
% 14 NEquip Inv
% 15 std(BMP)
% 16 Outwar Or
% 17 Bl Mkt Pm
% 18 Area
% 19 LatAmerica
% 20 SubSahara
% 21 High Enroll
% 22 %Publ Edu
% 23 Rev & Coup
% 24 War Dummy
% 25 Pol Rights
% 26 Civl Lib
% 27 Abs Lat
% 28 Age
% 29 Brit Col
% 30 Buddha
% 31 Catholic
% 32 Confuncious
% 33 EthnoL Frac
% 34 French Col
% 35 Hindu
% 36 Jewish
% 37 Muslim
% 38 Pr Exports
% 39 Protestants
% 40 Rule of Law
% 41 Spanish Col
% 42 Pop g
% 43 Work/Pop
% 44 Lab Force


long = growthley2(:,1);
lat = growthley2(:,2);
W = make_neighborsw(lat,long,3);

yi = growthley2(:,6);   % initial gdp level
y1 = growthley2(:,3);   % average gdp growth
y = y1*100;
y = studentize(y);
n = length(y);
xo = [yi growthley2(:,4:5) growthley2(:,21:26) growthley2(:,7:8) growthley2(:,12:15) growthley2(:,28) growthley2(:,38:40) growthley2(:,42:44)];
x = [ones(n,1) studentize(xo)];
[nobs,nvar] = size(x);

vnames = strvcat('ygrowth','constant','initial level','Primary School','life exp','high enrol','%public educ','rev+coup', ...
'War dummy','political rights','civil liberty','mining','econ organization','R FEX Dist ','Equip inv','NEquip inv','Std(bmp)', ...
'years open','age','Protestants','rule of law','pop growth','work/pop','labor force');


res = ols(y,x);
prt(res,vnames);
