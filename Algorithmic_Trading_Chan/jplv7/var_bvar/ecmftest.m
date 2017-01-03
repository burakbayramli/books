function ylevf = ecmftest(y,nlag,nfor,begf,r);
% PURPOSE: estimates an error correction model of order n
%          and produces f-step-ahead forecasts
%-------------------------------------------------------------
% USAGE:    yfor = ecmf(y,nlag,nfor,begf,r)
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast
%                  (defaults to length(y) + 1)
%              r = # of co-integrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                                 
%-------------------------------------------------------------
% NOTES: - constant vector automatically included
%        - x-matrix of exogenous variables not allowed
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator               
%---------------------------------------------------------------
% RETURNS:
%  yfor = an nfor x neqs matrix of level forecasts for each equation
%-------------------------------------------------------------
% SEE ALSO:  becmf, bvarf, varf, rvarf, recmf
%-------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

format long

[nobs neqs] = size(y);
% adjust nobs to feed the lags
nmin = min(nobs,begf-1);
nobse = nmin - nlag;

data = y;
nolags = nlag;
nocoint = r;

results = ecm(data,nolags,nocoint);     %ADAM's NOTE: call original ecm to get bmat

nx = 0;

if nargin == 5 % user supplied r-value
    % use johansen to determine ec variables
    % decrement r by 1 when calling johansen
    jres = johansen(y(1:nmin,:),0,nlag);
    % recover error correction vectors
    ecvectors = jres.evec;
    index = jres.ind; 
    % construct r-error correction variables
    x = mlag(y(1:nmin,index),1)*ecvectors(:,1:r); 
    [nobs2 nx] = size(x);
elseif nargin == 4 % we have to determine r-value
    jres = johansen(y(1:nmin,:),0,nlag);
    % find r = # significant co-integrating relations using
    % the trace statistic output
    trstat = jres.lr1;
    tsignf = jres.cvt;
    r = 0;
    for i=1:neqs;
        if trstat(i,1) > tsignf(i,2)
            r = i;
        end;
    end;
    % recover error correction vectors
    ecvectors = jres.evec;
    index = jres.ind; 
    % construct r error correction variables
    x = mlag(y(1:nmin,index),1)*ecvectors(:,1:r); 
    [nobs2 nx] = size(x); 
else
    error('Wrong # of input arguments to ecmf');
end;

% adjust nvar for constant term and error correction terms
k = neqs*nlag+nx+1;

yvec = zeros(nobse,1);
xvec = zeros(k,1);
bhat = zeros(k,1);

xmat = zeros(nobse,k);
ymat = zeros(nobse,neqs);
xnew = zeros(nlag+1,neqs);
bmat = zeros(k,neqs);
yfor = zeros(nfor,neqs);

% truncate to begf-1 for estimation
ytrunc = y(1:nmin,:);

% transform to 1st difference form
dy = zeros(nmin,neqs);
for i=1:neqs;
    dy(:,i) = ytrunc(:,i) - lag(ytrunc(:,i),1);
end;

% generate lagged rhs matrix
xlag = mlag(dy,nlag);

% add constant and ec variables to x-matrix and feed lags
if nx == 0
    xmat = [xlag(nlag+1:nmin,:) ones(nmin-nlag,1)];
else
    xmat = [xlag(nlag+1:nmin,:) x(nlag+1:nmin,:) ones(nmin-nlag,1)];
end;


% dimension some result matrices
bmat = zeros(k,neqs);
yfor = zeros(nfor,neqs);
ylev = zeros(nfor,neqs);
xlev = zeros(nfor,neqs);

for bmatcollect = 1:neqs
    bmat(:,bmatcollect) = results(1,bmatcollect).beta;  %ADAM's NOTE: use bmat from ecm call on line 44
end
% end of loop over equations 

% given bmat values generate future forecasts 

% 1-step-ahead forecast 
xtrunc = [dy(nmin-(nlag):nmin,:)
    zeros(1,neqs)];
xfor = mlag(xtrunc,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
if nx > 0
    ecterm = y(begf-1,index)*ecvectors(:,1:r); % add ec variables 
    xvec = [xobs ecterm 1];
else
    xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
    bhat = bmat(:,i);
    yfor(1,i) = xvec*bhat; % NOTE this is a change forecast
    ylev(1,i) = yfor(1,i) + y(nmin,i); % this adds the previous level %%%% ADAM's NOTE: adjusted nmin-1 to nmin
end;

ylevelfor = zeros(nfor,neqs);       %%%% ADAM's NOTE: set up matrix to take final level forecasts

ylevel=ylev(1,:);

ylevelfor(1,:) = ylevel;        %%%% ADAM's NOTE: put 1-step-ahead forecasts in

yforec=yfor(1,:);
lagdterms = zeros(nlag,neqs);

for filler = 1:nlag
    lagdterms(nlag+1-filler,:) = dy(nmin+1-filler,:);   %%%% ADAM's NOTE: set up lagged difference matrix
end

lagged_dif = lagdterms;

for steps = 2:nfor
    ecterm = ylevel*ecvectors(:,1:r);
    lagged_dif(1,:) = [];           %%%% ADAM's NOTE: when cycling through 2- to n-step ahead forecast, drop oldest lag difference and add previous forecast to lagged diff terms
    lagged_dif = [lagged_dif ; yforec];
    for reverse = 1:nlag
        lterm(reverse,:) = lagged_dif(nlag+1-reverse,:);    %%%% ADAM's NOTE: reorder the lagged terms to match that required on lines 173 and 177.
    end
    ltvec = lterm(:,1);
    for vect = 2:neqs
        ltvec = [ltvec ; lterm(:,vect)];
    end
    ltvect = transpose(ltvec);
    xvec1 = [ltvect ecterm 1];
    % loop over equations
for i=1:neqs;
    bhat = bmat(:,i);
    yfor1(1,i) = xvec1*bhat; % NOTE this is a change forecast
    ylev1(1,i) = yfor1(1,i) + ylevel(i); % this adds the previous level
end;
ylevel=ylev1;
yforec=yfor1;
ylevelfor(steps,:)=ylevel;
end

ylevf=ylevelfor;

