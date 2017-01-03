%   documentation for (1996 Presidential Election) election.dat
%   3,110 Counties
%
%   education variables are expressed as a proportion of all those with degrees,
%   that is as a proportion of: educated = (high+smcollege+associate+college+gradprof)
%   NOTE:because of this, the education proportion variables sum to unity
%
%   col 1= binary y with 0=Dole, 1=Clinton (1996 Presidential Election)
%   col2 = latt  coordinate
%   col3 = long  coordinate
%   col4 = log-urban  (log of urban population)
%   col5 = log-rural  (log of rural population)
%   col6 = prop-highs (pop with high school or GED graduates as a proportion of educated -- see above )
%   col7 = prop-smcollege  (pop with some college as a proportion of educated -- see above)
%   col8 = prop-associate (pop with associate degrees as a proportion of educated -- see above)
%   col9 = prop-college   (pop with college degrees as a proportion of educated -- see above)
%   col10 = prop-gradprof (pop with grad/professional degrees as a proportion of educated -- see above)
%   col11 = statecode (a number 1 to 48 with the state in which the county is located)

load election.dat;

y = election(:,1);
xc = election(:,2);
yc = election(:,3);
n = length(y);

xmat = [ones(n,1) election(:,4) election(:,7:10)];

vnames = strvcat('clinton/dole,s','constant','urban','some college', ...
'associate degree','college','graduate/professional');


result = probit(y,xmat);
prt(result,vnames);
