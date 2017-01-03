function ylevf = arf(y,nlag,nfor,begf,transf);
% PURPOSE: estimates a autoregression of order nlag
%          and produces f-step-ahead forecasts
%-------------------------------------------------------------
% USAGE: yfor = arf(y,nlag,nfor,begf,transf)
% where:    y    = an (nobs * 1) vector in levels
%           nlag = the lag length
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast
%                  (defaults to length(x) + 1)
%         transf = 0, no data transformation
%                = 1, 1st differences used to estimate the model
%                = freq, seasonal differences used to estimate
%                = cal-structure growth rates used to estimate
%                  e.g., cal(1982,1,12) [see cal() function]              
%-------------------------------------------------------------
% NOTE: constant term included automatically
%-------------------------------------------------------------
% RETURNS: 
%      yfor = an nfor x 1 vector of level forecasts 
%-------------------------------------------------------------
% SEE ALSO: ar, plt_ar, prt_ar
%-------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@spatial-econometrics.com

if nargin == 5 % user wants us to transform the data
 if isstruct(transf) % a growth rates transform
   tform = 2;
   freq = transf.freq;
   elseif transf == 0  % no transform
   tform = 0;
   elseif transf == 1  % 1st difference transform
   tform = 1;
   elseif (transf == 1) | (transf == 4) | (transf == 12)
   tform = 3;          % seasonal differences transform
   freq = transf;
   end;
elseif nargin == 4
tform = 0;
else
error('Wrong # of arguments to arf');
end;

[nobs neqs] = size(y);
% adjust nobs to feed the lags
nmin = min(nobs,begf-1);
% adjust nvar for constant term
k = nlag+1;

switch tform

case 1 % 1st differences transform

% transform data
dy = y - mlag(y,1);
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and 1st diff and end at begf-1
iota = trimr(iota,nlag+1,nobs-begf+1);
dys =  trimr(dy,nlag+1,nobs-begf+1);
xlag = trimr(xlag,nlag+1,nobs-begf+1);

% add constant to x-matrix
xmat = [xlag iota];

% end of 1st difference transformation case

case 2 % growth rates transformation

% transform data
dy = growthr(y,freq);
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and freq diff's and end at begf-1
iota = trimr(iota,nlag+freq,nobs-begf+1);
dys = trimr(dy,nlag+freq,nobs-begf+1);
xlag = trimr(xlag,nlag+freq,nobs-begf+1);

% add constant to x-matrix 
xmat = [xlag iota];

% end of growth-rates transform case

case 3 % seasonal differences transform

% transform data
dy = y - lag(y,freq);
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and freq diff's and end at begf-1
iota = trimr(iota,nlag+freq,nobs-begf+1);
dys = trimr(dy,nlag+freq,nobs-begf+1);
xlag = trimr(xlag,nlag+freq,nobs-begf+1);

% add constant to x-matrix 
xmat = [xlag iota];

otherwise  % case of no transformation

% generate lagged rhs matrix
xlag = mlag(y,nlag);
% constant term
iota = ones(nobs,1);
% truncate to feed lags and to end at begf-1 for estimation
dys  = trimr(y,nlag,nobs-begf+1);
dy   = y;
xlag = trimr(xlag,nlag,nobs-begf+1);
iota = trimr(iota,nlag,nobs-begf+1);

% add constant to x-matrix 
xmat = [xlag iota];

end; % end of data transformation cases

% dimension some result matrices
neqs = 1;
bmat = zeros(k,neqs);
yfor = zeros(nfor,neqs);

% ----- get bhat estimates

% save time by computing xpx only once
xpx = xmat'*xmat;

% pull out each y-vector and run regressions
for j=1:neqs;
 yvec = dys(:,j);
 bhat = (xpx)\(xmat'*yvec); 
 % save bhat
 bmat(:,j) = bhat;
end; 
% end of loop over equations


% given bhat estimates, generate future forecasts 
% These may be levels, 1st-differences, growth rates or seas diff's
% we worry transforming back to levels later

% 1-step-ahead forecast 
xtrunc = [dy(nmin-nlag:nmin,:)
          zeros(1,neqs)];
xfor = mlag(xtrunc,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
xvec = [xobs 1];


% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(1,i) = xvec*bhat;
end;

xnew = zeros(nlag+1,neqs);

% 2 through nlag-step-ahead forecasts
for step=2:nlag;

if step <= nfor

xnew(1:nlag-step+1,:) = dy(nmin-nlag+step:nmin,:);
xnew(nlag-step+2:nlag,:) = yfor(1:step-1,:);
xnew(nlag+1,:) = zeros(1,neqs);

xfor = mlag(xnew,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
xvec = [xobs 1];


% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step,i) = xvec*bhat;
end;

end;

end;

% nlag through nfore-step-ahead forecasts
for step=nlag:nfor-1;

if step <= nfor

cnt = step-(nlag-1);

 for i=1:nlag;
  xnew(i,:) = yfor(cnt,:);
  cnt = cnt+1;
 end;
 
xfor = mlag(xnew,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
xvec = [xobs 1];

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step+1,i) = xvec*bhat;
end;

end;

end;

% we now worry about transforming the forecasts back
% to levels

switch tform

case 1 % 1st differences forecasts
% convert 1st difference forecasts to levels
ylevf = zeros(nfor,neqs);
% 1-step-ahead forecast
ylevf(1,:) = yfor(1,:) + y(begf-1,:); % add change to actual from time t;
% 2-nfor-step-ahead forecasts
for i=2:nfor % 
ylevf(i,:) = yfor(i,:) + ylevf(i-1,:);
end;

% end of 1st differences case

case 2 % growth rates forecasts
% convert growth rate forecasts to levels
ylevf = zeros(nfor,neqs);
yfor = yfor/100.0; % growth-rates are mutiliplied by 100
for step=1:nfor;

if freq < step, % here we can use past level forecasts
   ylevf(step,:) = (1 + yfor(step,:)).*ylevf(step-freq,:);
else % case of freq > step, use past actual levels
   ylevf(step,:) = (1 + yfor(step,:)).*y(begf+step-freq-1,:);
end; % end of if freq <= step

end; % end of for step loop

case 3 % seasonal difference forecasts
% convert seasonal difference forecasts to levels

for step=1:nfor;

if freq < step, % here we use past level forecasts
   ylevf(step,:) = yfor(step,:) + ylevf(step-freq,:);
else % case of freq > step, use past actual levels
   ylevf(step,:) = yfor(step,:) + y(begf+step-freq-1,:);
end; % end of if freq <= step

end; % end of for step loop

otherwise % no transformation, so we have level forecasts already
ylevf = yfor;

end;


  
