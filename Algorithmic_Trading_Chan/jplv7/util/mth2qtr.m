function yqtr = mth2qtr(ymth,flag)
% PURPOSE: converts monthly time-series to quarterly averages
%---------------------------------------------------
% USAGE:  yqtr = mth2qtr(ymth)
% where:  ymth = monthly time series vector or matrix (nobs x k)
%         flag = 0 for averages (default) and 1 for sums
%---------------------------------------------------
% RETURNS: yqtr = quarterly time-series vector or matrix
%                 [floor(nobs/3) + 1] in length by k columns
%---------------------------------------------------
% NOTES:  the last observation is the actual month or
%         the average (sum) of the last 2 months in cases where
%         nobs/3 has a remainder               
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs qvar] = size(ymth);
denom = ones(1,qvar)*3.0;
robs = rem(nobs,3);
if nargin == 1
flag = 0;
end;

switch robs

case 0
cnt = 1;
yqtr = zeros(nobs/3,qvar);
for i=1:3:nobs-2
if flag == 0
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:))./denom;
else
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:));
end;
cnt = cnt+1;
end;

case 1
cnt = 1;
qobs = floor(nobs/3) + 1;
yqtr = zeros(qobs,qvar);
for i=1:3:nobs-2
if flag == 0
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:))./denom;
else
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:));
end;
cnt = cnt+1;
end;
yqtr(cnt,:) = ymth(nobs,:);

case 2
cnt = 1;
qobs = floor(nobs/3) + 1;
yqtr = zeros(qobs,qvar);
for i=1:3:nobs-2
if flag == 0
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:))./denom;
else
yqtr(cnt,:) = (ymth(i,:) + ymth(i+1,:) + ymth(i+2,:));
end;
cnt = cnt+1;
end;
if flag == 0
yqtr(cnt,:) = (ymth(nobs-1,:)+ymth(nobs,:))/2;
else
yqtr(cnt,:) = (ymth(nobs-1,:)+ymth(nobs,:));
end;

end