function ylevf = ecmf(y,nlag,nfor,begf,r);
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

[nobs neqs] = size(y);
% adjust nobs to feed the lags
nmin = min(nobs,begf-1);
nobse = nmin - nlag;

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

% save time by computing xpx only once
xpx = xmat'*xmat;

% pull out each y-vector and run regressions
for j=1:neqs;
    yvec = dy(nlag+1:nmin,j);
    bhat = (xpx)\(xmat'*yvec);
    
    % save bhat
    bmat(:,j) = bhat;
end; 
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
    ylev(1,i) = yfor(1,i) + y(nmin-1,i); % this adds the previous level
end;

xnew = zeros(nlag+nx+1,neqs);

% 2 through nlag-step-ahead forecasts
for step=2:nlag;
    
    if step <= nfor;
        
        xnew(1:nlag-step+1,:) = dy(nmin-nlag+step:nmin,:);
        xnew(nlag-step+2:nlag,:) = yfor(1:step-1,:);
        xnew(nlag+1,:) = zeros(1,neqs);
        
        
        xfor = mlag(xnew,nlag);
        [xend junk] = size(xfor);
        xobs = xfor(xend,:);
        % construct ec terms based on levels forecast from previous periods
        if nx > 0
            ecterm = ylev(step-1,index)*ecvectors(:,1:r);
            xvec = [xobs ecterm 1];
        else
            xvec = [xobs 1];
        end;
        
        
        % loop over equations
        for i=1:neqs;
            bhat = bmat(:,i);
            yfor(step,i) = xvec*bhat; % change forecast
            ylev(step,i) = yfor(step,i) + ylev(step-1,i); % level forecast
        end;
        
    end;
    
end;

% nlag through nfore-step-ahead forecasts
for step=nlag:nfor-1;
    
    if step <= nfor;
        
        cnt = step-(nlag-1);
        
        for i=1:nlag;
            xnew(i,:) = yfor(cnt,:);
            cnt = cnt+1;
        end;
        
        xfor = mlag(xnew,nlag);
        [xend junk] = size(xfor);
        xobs = xfor(xend,:);
        % construct ec terms based on levels forecast from previous periods
        if nx > 0
            ecterm = ylev(step,index)*ecvectors(:,1:r);
            xvec = [xobs ecterm 1];
        else
            xvec = [xobs 1];
        end;
        
        % loop over equations
        for i=1:neqs;
            bhat = bmat(:,i);
            yfor(step+1,i) = xvec*bhat; % change forecast
            ylev(step+1,i) = yfor(step+1,i) + ylev(step,i); % level forecast
            %CRASHES IF nlag==1  :    ylev(step+1,i) = yfor(step+1,i) + ylev(step-1,i); % level forecast
            % BDILLON CHANGED ylev(step-1 to ylev(step....
        end;
        
    end;
    
end;

% convert 1st difference forecasts to levels
ylevf = zeros(nfor,neqs);
% 1-step-ahead forecast
ylevf(1,:) = yfor(1,:) + y(begf-1,:); % add change to actual from time t;
% 2-nfor-step-ahead forecasts
for i=2:nfor % 
    ylevf(i,:) = yfor(i,:) + ylevf(i-1,:);
end;

