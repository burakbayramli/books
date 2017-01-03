function results = pfixed(y,index,x)
% PURPOSE: performs Fixed Effects Estimation for Panel Data
%          (for balanced or unbalanced data)using the within-groups 
%			  estimation procedure.
%----------------------------------------------------------------------------------------
% USAGE:  results = pfixed(y,index,x)
% where:    y     	= a (nobs x neqs) matrix of all of the individual's observations 
%						 		vertically concatenated. This matrix must include in the firt
%						 		column the dependent variable, the independent variables must follow
%						 		accordingly.	
%		    	index    = index vector that identifies each observation with an individual
%                  e.g. 1  (first 2 observations  for individual # 1)
%                       1
%                       2  (next  1 observation   for individual # 2)
%                       3  (next  3 observations  for individual # 3)
%                       3 
%                       3
%			 	x    		= optional matrix of exogenous variables, 
%						  		dummy variables. 				
%---------------------------------------------------------------------------------------
%RETURNS a structure
% results.meth  = 'pfixed'
% results.nobs  = nobs, # of observations
% results.nvar  = nvars, # of variables
% results.nid   =  # of observations per individual
% results.beta  = bhat 
% results.tstat = t-statistics 
% results.tprob = t-probabilities
% results.resid = residuals 
% results.yhat  = predicted values 
% results.y     = actual values 
% results.sige  = e'e/(n-k)
% results.rsqr  = r-squared
% results.rbar  = r-squared adjusted
% results.sige  = sigma^2 e
% results.xmat  = matrix of independent variables
% results.iintc = individual intercepts
% results.idy   = identity of individual;
% results.time  = time elapsed during the procedure
% results.crconst = correction of the constant term
%----------------------------------------------------------------------------------------

%Written by:
% Carlos Alberto Castro
% National Planning Department
% Bogota, Colombia
% Email: ccastro@dnp.gov.co 

%****************************************************************************************
% NOTE: James P. LeSage provided corrections on the creation and use of the index
%       vector used to identify the the individual's observations.
%****************************************************************************************

t0 = clock;

results.meth = 'pfixed';

[nobs equ]= size(y);

nx = 0;

if nargin == 3 
[nobs2 nx] = size(x);
 if (nobs2 ~= nobs)
 error('nobs in x-matrix not the same as y-matrix');
 end;
end;

results.nobs = nobs;

% creation of the id matrix using the vector index

nindiv = length(unique(index));
id = zeros(nindiv,3);    
id(:,1) = unique(index); 
for i=1:nindiv
    id(i,2) = length(find(index == i));
end;
id(:,3) = cumsum(id(:,2));

results.nid = id(:,2);
results.idy =id(:,1);
results.crconst = 1; %correction of the constant term

% tranformation of all the variables used
% the variables are expressed as deviations from the individual means
[n u]= size(id);

i=1;

for j=1:n

	while i<=id(j,3),
   
   ytemp= y(i:id(j,3),:);
   
   medias(j:id(j,1),:)= mean(ytemp);
   madj(i:id(j,3),:)= ytemp-(ones(id(j,2),1)*mean(ytemp));
       
   i= i+ id(j,2);      
   
	end;
end;
  
y= madj;

% form x-matrix
if nx 
xmat = [y(:,2:equ) x];
else
xmat = [y(:,2:equ)];
end;

[nobs3 nvars]= size(xmat); 
results.nvar  = nvars;
results.xmat  = xmat;

% run OLS

 res = ols(y(:,1),xmat);
 results.beta  = res.beta;      % bhats
 results.tstat = res.tstat;     % t-stats
% compute t-probs
      tstat = zeros(nvars,1);
      tstat = res.tstat;
      tout = tdis_prb(tstat,nobs-nvars);
 results.tprob = tout;          % t-probs
 results.resid = res.resid;     % resids 
    sigu = res.resid'*res.resid; %sse
 results.yhat = res.yhat;       % yhats
   results.y    = y(:,1);           % actual y
   results.rsqr = res.rsqr;       % r-squared
   results.rbar = res.rbar;       % r-adjusted
   results.sige = res.sige;       % sigma e
   
   
% individual intercepts
if nx 
   results.iintc = medias(:,1)-(medias(:,2:equ)*(results.beta(:,1:equ)));    
else
   results.iintc = medias(:,1)-(medias(:,2:equ)*(results.beta));    
end;

results.time = etime(clock,t0); % time elapsed during the procedure





