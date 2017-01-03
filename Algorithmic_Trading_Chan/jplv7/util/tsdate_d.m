% PURPOSE: demonstrate tsdate functions
%
%
% usage: tsdate_d

freq = 4;
begyr = 1922;
begq = 2;

for i=1:12
tsdate(begyr,begq,freq,i)
end;

freq = 1;
begyr = 1922;
begper = 1;

for i=1:12
tsdate(begyr,begper,freq,i)
end;

freq = 12;
begyr = 1922;
begmth = 12;

for i=1:12
tsdate(begyr,begmth,freq,i)
end;
