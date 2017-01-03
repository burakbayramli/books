% PURPOSE: A timing comparison of heteroscedastic
%          versus homoscedastic on small and large
%          datasets                    
%---------------------------------------------------
% USAGE: far_timing
%---------------------------------------------------

clear all;
load anselin.dat; % small 49-observation dataset
xc = anselin(:,4);
yc = anselin(:,5);
y = anselin(:,1);
[j1 W j2] = xy2cont(xc,yc);
ydev = y - mean(y); % put data into deviations from means form

times = zeros(2,2);

ndraw = 2000;
nomit = 500;
prior.rval = 4; % heteroscedastic prior
result = far_g(ydev,W,ndraw,nomit,prior); 
times(1,1) = result.time; % matlab version

prior.novi = 1; % homoscedastic prior
result = far_g(ydev,W,ndraw,nomit,prior); 
times(1,2) = result.time; % matlab version

% NOTE a large data set with 3107 observations
% from Pace and Barry

load elect.dat;             % load data on votes
y = log(elect(:,7)./elect(:,8)); % proportion of voters casting votes
xc = elect(:,5);
yc = elect(:,6);
[j W j] = xy2cont(xc,yc);
ydev = y - mean(y);         % deviations from the means form 
prior.rval = 4; % heteroscedastic prior
result = far_g(ydev,W,ndraw,nomit,prior);
times(2,1) = result.time;

prior.novi = 1;
result = far_g(ydev,W,ndraw,nomit,prior);
times(2,2) = result.time;

in.cnames = strvcat('heteroscedastic','homoscedastic');
in.rnames = strvcat('time in seconds','49 observations','3,107 observations');
fprintf('time for 2,000 draws \n');
mprint(times,in);

