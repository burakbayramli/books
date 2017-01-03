function yyr = qtr2yr(yqtr,flag)
% PURPOSE: converts quarterly time-series to annual averages
%---------------------------------------------------
% USAGE:  yyr = qtr2yr(yqtr)
% where:  yqtr = quarterly time series vector or matrix (nobs x k)
%         flag = 0 for averages (default) and 1 for sums
%---------------------------------------------------
% RETURNS: yyr = annual time-series vector or matrix
%                 [floor(nobs/4) + 1] in length by k columns
%---------------------------------------------------
% NOTES:  the last observation is the actual quarter or
%         the average (sum) of the last 2 or 3 quarters in cases where
%         nobs/4 has a remainder               
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[nobs qvar] = size(yqtr);
denom = ones(1,qvar)*4.0;
robs = rem(nobs,4);
if nargin == 1
flag = 0;
end;

switch robs

case 0
cnt = 1;
yyr = zeros(nobs/4,qvar);
for i=1:4:nobs-3
if flag == 0
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:))./denom;
else
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:));
end;
cnt = cnt+1;
end;

case 1
cnt = 1;
qobs = floor(nobs/4) + 1;
yyr = zeros(qobs,qvar);
for i=1:4:nobs-3
if flag == 0
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:))./denom;
else
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:));
end;
cnt = cnt+1;
end;
yyr(cnt,:) = yqtr(nobs,:);

case 2
cnt = 1;
qobs = floor(nobs/4) + 1;
yyr = zeros(qobs,qvar);
for i=1:4:nobs-3
if flag == 0
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:))./denom;
else
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:));
end;
cnt = cnt+1;
end;
if flag == 0
yyr(cnt,:) = (yqtr(nobs-1,:) + yqtr(nobs,:))/2;
else
yyr(cnt,:) = (yqtr(nobs-1,:)+yqtr(nobs,:));
end;

case 3
cnt = 1;
qobs = floor(nobs/4) + 1;
yyr = zeros(qobs,qvar);
for i=1:4:nobs-3
if flag == 0
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:))./denom;
else
yyr(cnt,:) = (yqtr(i,:) + yqtr(i+1,:) + yqtr(i+2,:) + yqtr(i+3,:));
end;
cnt = cnt+1;
end;
if flag == 0
yyr(cnt,:) = (yqtr(nobs-1,:) + yqtr(nobs,:) + yqtr(nobs,:))/3;
else
yyr(cnt,:) = (yqtr(nobs-1,:)+yqtr(nobs,:)+yqtr(nobs,:));
end;


end