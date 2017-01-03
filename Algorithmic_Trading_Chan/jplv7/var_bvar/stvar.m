function results = stvar(y, param, x)
% PURPOSE: performs a smooth transition vector autoregression
%---------------------------------------------------
% USAGE:  result = stvar(y, param, x)
% where:    y    = an (nobs x neqs) matrix of y-vectors
%        
%           y should be fixed from most endogenous to most exogenous
%
%           Param is a 1x10 vector that includes the following 
%           information in order:
%
%           nlag = the lag length
%         shockv = variable of y being shocked (column of y)
%          trans = position of the transition variable in the y matrix
%         thresh = the threshold value
%         smooth = the smoothnes parameter                
%          shock = the standard deviation shocks 
%            sim = the number of montecarlo simulations
%          IRper = the number of periods for the impulse response function
%        history = 0 if choosen period under threshold (1 if not)
%       translag = the lag of the transition variable  
%
%          param = [nlag shockv trans thresh smooth shock sim IRper history translag]
%
%           (e.g.)-> param=[1 2 1 0 1 1 1000 24 1 1]
%
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%
%       NOTE: Smooth transition function is not applied to exogenous
%             variables
%
%---------------------------------------------------
%
%      Bibliography:
%
%      The non linear VAR is built on the following structure (reduced form): 
%
%      Y(t)=[(A-B(g(t))L]Y(t)+E(t) 
%
%      where  
%
%      g(t)=is the logistic form depending on a state variable
%
%      and L is a Lag Operator
%
%       For furtherings check out Wiese 1999
%---------------------------------------------------
% RETURNS a structure
% results.meth = 'STVAR'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
%
% Generalized Impulse Response Function (see Pesaran, Potter & Koop (1997))
%
% results.irfs      = Generalized Impulse Response Function (GIRF)
% results.irfs_sup  = Superior Band of the GIRF
% results.irfs_inf  = Inferior Band of the GIRF
%
% Non-Linearity Tests (see Terasvirta (1995))
%
% results.omega0    = Var-Cov of the equivalent linear VAR
% results.omega1    = Var-Cov;
% results.LR        = tests.LR;
% results.LRpval    = tests.LRpval;
%
% take into account that results (for parameters and coefficients are presented in the following order):   
% 
% y1(t-1), y1(t-2),....y1(t-nlag),y2(t-1)....ynvar(t-nlag),y1(t-1),
% g*y1(t-2),....g*y1(t-nlag),g*y2(t-1)....g*ynvar(t-nlag)
%
%---------------------------------------------------
% SEE ALSO: vare, varf, prt_var, prt_granger, prt_ftests (from LeSage's econometrics
% toolbox) and optsmooth (from Saki's toolkit)
%
%---------------------------------------------------
%
% written by:
% Saki Bigio 
% Department of Macroeconomic Analysis, 
% Banco Central de Reserva del Peru
% Paul de Beaudiez 530,
% Lima L27,  PERU
% sbigio@bcrp.gob.pe

results.meth='Smooth Transition VAR';

[nobs neqs] = size(y);

results.nobs = nobs; % # of observations
results.neqs = neqs; % # of equations

%Setting Default Values
nlag   = param(1);
if nlag ==0
    nlag=1;
end;

results.nlag = nlag; % # of lags

shockv   = param(2);
if shockv == 0
    shockv = neqs;
end;

trans    = param(3);
if trans == 0
    trans = 1;
end;

thresh   = param(4);
if thresh == 0
    thresh = 0;
end;

smooth   = param(5);
if smooth == 0
    smooth = 7;
end;

shock    = param(6);
if shock == 0
    shock = 1;
end;

sim      = param(7);
if sim == 0
    sim   = 1000;
end;

IRper    = param(8);
if IRper == 0
    IRper = 24;
end;

Hist     = param(9);
if Hist == 0 | Hist == 1
else
    error('choose a type of history');
end;

translag = param(10);
if translag == 0
    translag = 1;
end;

if nargin == 3
 [nobsx nx] = size(x);
 if (nobsx ~= nobs)
  error('var: nobs in x-matrix not the same as y-matrix');
 end;
results.nvar = 2*nlag*neqs+nx+2; % # of variables per equation
end;

results.nvar = 2*nlag*neqs+2;    % # of variables per equation

%Building the Smooth Transition Function
transi = y(:,trans);
standard=std(transi);
transi = lag(transi,translag);
gfunc = (1+exp(-smooth*(transi-thresh)/standard)).^(-1); %Weise uses the function with -1/2, note that variables aren't normalized
g=gfunc;

%Generalizing nx for case when we have exog. var.
nx = 0;

%Building the non-linear data matrix
gy=[]; 
ylag=mlag(y,nlag);

for i=1:neqs*nlag;
gy=[gy g.*ylag(:,i) ];
end    

if nargin == 3
gy=[gy x];
end;  

%Running the Non-Linear VAR 
varres=vare2(y,nlag,gy);

%Loading Variables for Impulse Response
n = varres(1).nobs;
k = varres(1).neqs;
gy=gy((1+nlag:nobs),:);
g=g((1+nlag:nobs),:);

%Estimation Coefficients 
b=[];
for i=1:neqs          
    b  = [b varres(i).beta];
end

%Estimation T-Stats
tstat=[];
for i=1:neqs          
    tstat  = [tstat varres(i).tstat];
end

%Estimation P-Values
tprob=[];
for i=1:neqs          
    tprob  = [tprob varres(i).tprob];
end

%Estimation Residuals
e = zeros(n,k);
for i=1:k
    e(:,i) = varres(i).resid;
end

% Recovering Predicted Values
yhat=[];
for i=1:neqs          
    yhat  = [yhat varres(i).yhat];
end

% Recovering Alligned Actual Values
yact=[];
for i=1:neqs          
    yact  = [yact varres(i).y];
end

% Recovering R-squared
rsqr=[];
for i=1:neqs          
    rsqr  = [rsqr varres(i).rsqr];
end

% Recovering Alligned Actual Values
yact=[];
for i=1:neqs          
    yact  = [yact varres(i).y];
end

% Recovering R-Squared Adjusted
rbar=[];
for i=1:neqs          
    rbar  = [rbar varres(i).rbar];
end

%registering relevant statistics of the estimation
results.beta  = b;
results.tstat = tstat;
results.tprob = tprob;
results.resid = e;
results.yhat  = yhat ;
results.y     = yact;
results.rsqr  = rsqr;
results.rbar  = rbar;

%Extract reduced form residuals and (using Cholesky decomposition, obtain
%functional form uncorrelated errores)
Omega = (1/(n-k))*e'*e;
C = chol(Omega);
v = inv(C)*e';
v=v';

%Creating empty matrix to store differenced paths
paths_dif=[];

%Preparing x
if nargin == 3
        x=x((nlag+1:nobs),:);    
end

%Starting main simulation loop
for i = 1:sim

    %Choosing a Random period that coincides with the relevant history
    junk=randint(1,1,nlag+1, nobs-2*IRper);
    
    if Hist ==1
        while y(junk-translag,trans) < thresh
              junk=randint(1,1,nlag+1, nobs-2*IRper);
        end
        History=junk;
    else
        while y(junk-translag,trans) > thresh
              junk=randint(1,1,nlag+1, nobs-2*IRper);
        end
        History=junk;
    end
        
    %Reshufling uncorrelated errors sim times (montecarlo simulations)
    %and fixing size

    temp = bootstrp(1,'equal',v);
    vbs = reshape(temp,n,k);
    
    %Fix a given shock because we are using Cholesky decomposition, a direct addition can be
    %done that will be equal to fixing a "shock" sized standard deviation shock
    %(see Hamilton p. 400)
    
    vbss=vbs;
    vbss(History,shockv)= shock; %???this is only the shock, should we add it to the residual...?  
    vbs(History,shockv)= 0;
    
    %returning dependence to shocks

    ebs_  = (C*vbs')';
    ebss_ = (C*vbss')';
    eb    = [ebs_ ebss_];

    %Setting an auxiliary variable and all complements to rebuild series
    paths=[];
    for i=0:neqs:neqs
    
    
        ebs=eb(:,i+1:i+neqs);
        path = y;
        g_sim=gfunc((1+nlag:nobs),:);
    
        ylag = mlag(y,nlag);
        ymat=ylag(nlag+1:nobs,:);

        gy=[];
        for i=1:neqs*nlag   
            gy=[gy g_sim.* ymat(:,i)]; 
        end   
    
        if nargin == 3
            gy=[gy x];    
        end; 
       
        gy=[ymat gy ones(nobs-nlag,1)];

        %Rebuilding series for I-R period
        for t=History:History+IRper; % (le quitamos 
            path(t,:)= gy(t,:)*b + ebs(t,:);

            %updating gy matrix with the values just obtained
            for j = 0:k-1
                ymat(t+1,j*nlag+1:(j+1)*nlag)=rot90(path((t-nlag+1):t,j+1),-1); %check this out (t or t+1)
            end;

            %Updating point t of the G function 
            g_sim(t) = (1+exp(-smooth*(path(t-translag,trans)-thresh)/standard))^(-1);

            %Emptying GY matrix and updating it
            gy=[];

            for i=1:neqs*nlag
                gy=[gy g_sim.*ymat(:,i)];
            end;    
        
            if nargin == 3
                gy=[gy x];    
            end;
    
            gy=[ymat gy ones(nobs-nlag,1)]; 
    
        end;
    
        path=path((History):(History+IRper),:); 
        %store resulting path
        paths=[paths path];
    
    end;

    paths_us  = paths(:,1:neqs);
    paths_s   = paths(:,neqs+1:2*neqs);
    paths_dif = [paths_dif paths_s-paths_us];

end;  

%Reordering main loop's output

for j=1:neqs
    for i=1:sim
    IRFS_aux(:,i+(j-1)*sim)=paths_dif(:,j+neqs*(i-1));
    end;
end;


%Building I-R and confidence bands over the 90%
inf(IRper+1,neqs)=0;
sup(IRper+1,neqs)=0;

for i=1:neqs
    for t=1:IRper+1
        IRF(t,i)=mean(IRFS_aux(t,(1+(i-1)*sim):i*sim));
        [prob val]=ecdf(IRFS_aux(t,(1+(i-1)*sim):i*sim));
        inf(t,i)=val(min(find(prob>0.05)));
        sup(t,i)=val(min(find(prob>=0.95)-1));
    end;
end;

results.irfs=IRF;
results.irfs_sup=sup;
results.irfs_inf=inf;

%Graphing Impulse Response Function
periods=1:IRper+1;

for j=1:neqs
    subplot(neqs,1,j);
    plot(periods,sup(:,j),'r',periods,inf(:,j),'r',periods,IRF(:,j),'b-',periods,0,'k');
    xlabel('Periods');
    ylabel('variable ?');
end

%compute tests statistics for non-linearity using stvar_tests
tests=stvar_tests(y,param);

results.fstat_pval=tests.fstat_pval;
results.omega0=tests.omega0;
results.omega1=tests.omega1;
results.LR=tests.LR;
results.LRpval=tests.LRpval;

function a = randint(m,n,a,b)
% RANDINT Randomly generated integral matrix
%	randint(m,n) returns an m-by-n matrix with entries between
%	0 and 9.
%	randint(m,n,a,b) returns entries between a and b.
if nargin < 3, a = 0; b = 9; end
a = floor((b-a+1)*rand(m,n))+ a;

function [bootstat, bootsam] = bootstrp(nboot,bootfun,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10);
%BOOTSTRP Bootstrap statistics.
%   BOOTSTRP(NBOOT,BOOTFUN,D1,...) draws NBOOT bootstrap data samples and
%   analyzes them using the function, BOOTFUN. NBOOT must be a positive integer.
%   BOOTSTRAP passes the (data) D1, D2, etc. to BOOTFUN.
%
%   [BOOTSTAT,BOOTSAM] = BOOTSTRP(...) Each row of BOOTSTAT contains
%   the results of BOOTFUN on one bootstrap sample. If BOOTFUN returns a matrix,
%   then this output is converted to a long vector for storage in BOOTSTAT.
%   BOOTSAM is a matrix of indices into the row

%   Reference:
%      Efron, Bradley, & Tibshirani, Robert, J.
%      "An Introduction to the Bootstrap", 
%      Chapman and Hall, New York. 1993.

%   B.A. Jones 9-27-95
%   Copyright (c) 1993-98 by The MathWorks, Inc.
%   $Revision: 2.5 $  $Date: 1997/11/29 01:44:59 $

% Initialize matrix to identify scalar arguments to bootfun.
scalard = zeros(nargin-2,1);

lp = '(';      % Left parenthesis string variable.
rp = ')';      % Right parenthesis string variable.
c  = ',';      % Comma string variable.
ds = 'd';      % 'd' as a string variable.

% Construct argument list for bootfun
for k = 3:nargin
   dk = [ds,num2str(k-2)];
   [row,col] = size(eval(dk));
   if max(row,col) == 1
      scalard(k-2) = 1;
   end
   if row == 1 & scalard(k-2) == 0
      eval([dk,'=',dk,'(:);']);
     row = col;
   end
   if k == 3
      pstring = [lp,dk];
     n = row;
     if nargin == 3
        pstring = [pstring,rp];
      end
   elseif k == nargin & nargin > 3
      pstring = [pstring,c,dk,rp];
   else
      pstring = [pstring,c,dk];
   end
end

% Create index matrix of bootstrap samples.
bootsam = unidrnd(n,n,nboot);

% Get result of bootfun on actual data and find its size. 
thetafit = eval([bootfun,pstring]);
[ntheta ptheta] = size(thetafit);

% Initialize a matrix to contain the results of all the bootstrap calculations.
bootstat = zeros(nboot,ntheta*ptheta);

dbs = 'db';

% Do bootfun - nboot times.
for bootiter = 1:nboot

   for k = 3:nargin
      dk  = [ds,num2str(k-2)];
      dbk = [dbs,num2str(k-2)];
     if scalard(k-2) == 0
         eval([dbk,'=',dk,'(bootsam(:,',num2str(bootiter),'),:);']);
     else
         eval([dbk,'=',dk,';']);
     end
     
      if k == 3
         pstring = [lp,dbk];
        n = row;
        if nargin == 3
           pstring = [pstring,rp];
         end
      elseif k == nargin & nargin > 3
         pstring = [pstring,c,dbk,rp];
      else
         pstring = [pstring,c,dbk];
      end
   end

   evalstr = [bootfun,pstring];
   tmp = eval(evalstr);
   bootstat(bootiter,:) = (tmp(:))';
end

function e = equal(x)

e=x;

function results=stvar_tests(y,param,x)

% PURPOSE: performs a smooth transition vector autoregression
% only presenings tests
%---------------------------------------------------
% USAGE:  result = stvar2(y, param, x)
% where:    y    = an (nobs x neqs) matrix of y-vectors
%        
%           y should be fixed from endog to exog
%
%           Param is a 1x9 vector that includes the following 
%           information in order:
%
%           nlag = the lag length
%         shockv = variable of y being shocked (column of y)
%          trans = position of the transition variable in the y matrix
%         thresh = the threshold value
%         smooth = the smoothnes parameter                
%          shock = the standard deviation shocks 
%            sim = the number of montecarlo simulations
%          IRper = the number of periods for the impulse response function
%        history = the period we want to atart with        
%      transilag = the lag of the transition variable 
%          
%          param = [nlag shockv trans thresh smooth shock sim IRper history transilag]
%
%           (e.g)-> param=[1 2 1 0 1 1 1000 24 30 1]
%
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%
%       NOTE: Smooth transition function is not applied to exogenous
%             variables
%
%
%---------------------------------------------------
% RETURNS a structure
% results.meth = 'STVAR'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
% results.nvar = nlag*neqs+nx+1, # of variables per equation
% --- the following are referenced by equation # --- 
% results(eq).beta  = bhat for equation eq
% results(eq).tstat = t-statistics 
% results(eq).tprob = t-probabilities
% results(eq).resid = residuals 
% results(eq).yhat  = predicted values 
% results(eq).y     = actual values 
% results(eq).sige  = e'e/(n-k)
% results(eq).rsqr  = r-squared
% results(eq).rbar  = r-squared adjusted
% results(eq).boxq  = Box Q-statistics
% results(eq).ftest = Granger F-tests
% results(eq).fprob = Granger marginal probabilities
%---------------------------------------------------
% SEE ALSO: 
%vare, varf, prt_var, prt_granger, prt_ftests (from LeSage's econometrics
% toolbox)
%
%stvar2, stvar_search (from Saki's Toolkit)
%
%---------------------------------------------------

% written by:
% Saki Bigio, Macroeconomic Analysis Dept.
% Banco Central de Reserva del Peru
% Paul de Beaudiez 530,
% Lima L27,  PERU
% sbigio@bcrp.gob.pe

results.meth='STVAR_tests';

[nobs neqs] = size(y);

results.nobs = nobs; % # of observations
results.neqs = neqs; % # of equations

%Setting Default Values
nlag   = param(1);
if nlag ==0
    nlag=1;
end;

results.nlag = nlag; % # of lags

shockv = param(2);
if shockv == 0
    shockv = neqs;
end;

trans  = param(3);
if trans == 0
    trans = 1;
end;

thresh = param(4);
if thresh == 0
    thresh = 0;
end;

smooth = param(5);
if smooth == 0
    smooth = 7;
end;

shock  = param(6);
if shock == 0
    shock = 1;
end;

sim    = param(7);
if sim == 0
    sim   = 1000;
end;

IRper  = param(8);
if IRper == 0
    IRper = 24;
end;

Hist   = param(9);
if Hist == 0
    Hist = nlag+1;
end;

transilag = param(10);
if transilag == 0
    transilag = 1;
end;

if nargin == 3
 [nobsx nx] = size(x);
 if (nobsx ~= nobs)
  error('var: nobs in x-matrix not the same as y-matrix');
 end;
results.nvar = 2*nlag*neqs+nx+2; % # of variables per equation
end;

results.nvar = 2*nlag*neqs+2*1; % # of variables per equation

%Building the Smooth Transition Function
transi   = y(:,trans);
standard = std(transi);
transi   = lag(transi,transilag);
gfunc    =(1+exp(-smooth*(transi-thresh)/standard)).^(-1); %Weise uses the function with -1/2
g        = transi;

%Generalizing nx for case when we have exog. var.
nx = 0;

%Building the non-linear data

gy=[]; 
ylag=mlag(y,nlag);

for i=1:neqs*nlag
gy=[gy g.*ylag(:,i) ];
end    

if nargin == 3
gy=[gy x];
end;  

%We should fix the first period cause of the lag and SMTV
varres=vare2(y,nlag,gy);

%Loading Variables for Impulse Response
n = varres(1).nobs;
k = varres(1).neqs;

%get coefficients %%%%%%Generalize
b=[];
for i=1:neqs          
    b  = [b varres(i).beta];
end

%Extract reduced form residuals and (using Cholesky decomposition, obtain
%functional form uncorrelated errores)
e = zeros(n,k);
for i=1:k
    e(:,i) = varres(i).resid;
end

%compute tests statistics for non-linearity
%start with the equation by equation test
varres_aux=vare2(y,nlag);

res_r=zeros(n,k);
for i=1:k
    res_r(:,i) = varres_aux(i).resid;   %get restricted residuals
end;

res_ur=e;                                %get unrestricted residuls

%Calculating the Equation by Equation LM estimator
FStat_eq=zeros(neqs,1);
for i=1:neqs
    SSR0=sum(res_r(:,i).^2);
    SSR1=sum(res_ur(:,i).^2);
    FStat_eq(i)=((SSR0-SSR1)/(nlag*neqs))/((SSR1/((nobs-nlag)-(2*nlag*neqs+1))));%calculate small sample F-Statistic
end;

pval=1-fdis_prb(FStat_eq,nlag*neqs,(nobs-nlag)-(2*nlag*neqs+1));        %calculate corresponding p-value
results.fstat_pval=pval;

%the overall LR-test

%omega0= (1/((nobs-nlag)-neqs*nlag-1))*res_r'*res_r;                  %build cov. matrices
%omega1= (1/((nobs-nlag)-2*neqs*nlag-1))*res_ur'*res_ur;

%LR=((nobs-nlag)-nlag*neqs-1)*(log(det(omega0))-log(det(omega1)));
%LRpval=1-chis_prb(LR,nlag*neqs^2);

omega0= (1/((nobs-nlag)))*res_r'*res_r;                  %build cov. matrices
omega1= (1/((nobs-nlag)))*res_ur'*res_ur;

LR=(nobs-nlag)*(log(det(omega0))-log(det(omega1)));
LRpval=1-chis_prb(LR,(nlag*neqs^2));

results.omega0=omega0;
results.omega1=omega1;      
results.LR=LR;
results.LRpval=LRpval;
results.beta=b;
results.g=g;


function results = vare2(y,nlag,x)
% PURPOSE: performs vector autogressive estimation and presents no tests
%---------------------------------------------------
% USAGE:  result = vare(y,nlag,x) 
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%---------------------------------------------------
% RETURNS a structure
% results.meth = 'vare'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
% results.nvar = nlag*neqs+nx+1, # of variables per equation
% --- the following are referenced by equation # --- 
% results(eq).beta  = bhat for equation eq
% results(eq).tstat = t-statistics 
% results(eq).tprob = t-probabilities
% results(eq).resid = residuals 
% results(eq).yhat  = predicted values 
% results(eq).y     = actual values 
% results(eq).sige  = e'e/(n-k)
% results(eq).rsqr  = r-squared
% results(eq).rbar  = r-squared adjusted
% results(eq).boxq  = Box Q-statistics
% results(eq).ftest = Granger F-tests
% results(eq).fprob = Granger marginal probabilities
%---------------------------------------------------
% SEE ALSO: varf, prt_var, prt_granger, prt_ftests 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

results(1).meth = 'vare2';

nx = 0;

if nargin == 3
[nobs2 nx] = size(x);
 if (nobs2 ~= nobs)
 error('vare2: nobs in x-matrix not the same as y-matrix');
 end;
end;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term 
 k = neqs*nlag + 1 + nx;
 nvar = k;

results(1).nvar = nvar;

xlag = mlag(y,nlag);

results(1).nobs = nobse;
results(1).neqs = neqs;
results(1).nlag = nlag;


% form x-matrix
if nx 
xmat = [xlag(nlag+1:nobs,:) x(nlag+1:nobs,:) ones(nobs-nlag,1)];
else
xmat = [xlag(nlag+1:nobs,:) ones(nobs-nlag,1)];
end;


% pull out each y-vector and run regressions
for j=1:neqs;

 yvec = y(nlag+1:nobs,j);
 res = ols(yvec,xmat);
 results(j).beta  = res.beta;      % bhats
 results(j).tstat = res.tstat;     % t-stats
 % compute t-probs
      tstat = zeros(nvar,1);
      tstat = res.tstat;
      tout = tdis_prb(tstat,nobse-nvar);
 results(j).tprob = tout;          % t-probs
 results(j).resid = res.resid;     % resids 
    sigu = res.resid'*res.resid;
 results(j).yhat = res.yhat;       % yhats
   results(j).y    = yvec;           % actual y
   results(j).rsqr = res.rsqr;       % r-squared
   results(j).rbar = res.rbar;       % r-adjusted
   results(j).sige = res.sige;


% do the Q-statistics
% use residuals to do Box-Pierce Q-stats
% use lags = nlag in the VAR
% NOTE: a rule of thumb is to use (1/6)*nobs
%       but this seems excessive to me
elag = mlag(res.resid,nlag);
% feed the lags
etrunc = elag(nlag+1:nobse,:);
rtrunc = res.resid(nlag+1:nobse,1);
qres   = ols(rtrunc,etrunc);
if nlag ~= 1
 boxq   = (qres.rsqr/(nlag-1))/((1-qres.rsqr)/(nobse-nlag));
else
 boxq   = (qres.rsqr/(nlag))/((1-qres.rsqr)/(nobse-nlag));
end;

results(j).boxq = boxq;

% form x matrices for joint F-tests
% exclude each variable from the model sequentially

for r=1:neqs;
xtmp = [];
for s=1:neqs;
 if s ~= r
   xlag = mlag(y(:,s),nlag);
 xtmp = [xtmp trimr(xlag,nlag,0)];
   end;
end;
% we have an xtmp matrix that excludes 1 variable
% add deterministic variables (if any) and constant term
if nx > 0
xtmp = [xtmp x(1:nobse,:) ones(nobse,1)];
else
xtmp = [xtmp ones(nobse,1)];
end;
% get ols residual vector
b = xtmp\yvec; % using Cholesky solution
etmp = yvec-xtmp*b;
sigr = etmp'*etmp;
% joint F-test for variables r
ftest(r,1) = ((sigr - sigu)/nlag)/(sigu/(nobse-k)); 
end;

results(j).ftest = ftest;     
%results(j).fprob = fdis_prb(ftest,nlag,nobse-k);

end; 
% end of loop over equations 
 



