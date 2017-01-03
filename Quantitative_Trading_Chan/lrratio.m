function lrratio(y,maxlag,minlag,simsc,x)
% PURPOSE: performs likelihood ratio test for var model
%          to determine optimal lag length
%---------------------------------------------------
% USAGE:  lrratio(y,maxlag,minlag,simsc,x) 
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           maxlag = the maximum lag length
%           minlag = the minimum lag length
%            simsc = flag for Sim's dof correction factor 
%                    0 = no, 1 = use correction
%                    (default = 0)
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%---------------------------------------------------
% RETURNS: nothing, prints results to the MATLAB command window
%---------------------------------------------------
% SEE ALSO: var, varf, prt_var 
%---------------------------------------------------

if nargin > 5
error('wrong # of arguments to lrratio');
elseif nargin == 5
xflag = 1;
elseif nargin == 4
xflag = 0;
elseif nargin == 3
simsc = 0;
xflag = 0;
end;

if maxlag < minlag
 error('maxlag < minlag in lrratio');
end;

if minlag  < 1
 minlag = 1;
end;

[nobs neqs] = size(y);


% loop over lag lengths and do likelihood ratio tests
for i=maxlag:-1:minlag+1
 
 % adjust nobs to feed the lags
    nobse = nobs-i;

    if xflag == 1 % case of deterministic variables
       if i == maxlag
  resid1 = var_resid(y,i,x);
  resid2 = var_resid(y,i-1,x); % restricted model
    else
  resid1 = resid2; % save time by not running it again
  resid2 = var_resid(y,i-1,x);
     end;
   else % case of no deterministic variables
       if i == maxlag
  resid1 = var_resid(y,i);   
  resid2 = var_resid(y,i-1); % restricted model
    else
  resid1 = resid2; % save time by not running it again
  resid2 = var_resid(y,i-1);
    end;
 end; 
 
 % compute likelihood ratio test
 
 % first get var-cov matrices for residuals
 epe1 = cov(resid1); % cov is a MATLAB command
 epe2 = cov(resid2);
 
 if simsc == 1
    tminusc = nobse-neqs*i+1;
    else
    tminusc = nobse;
    end;
   
 % compute (T-c)*(log(det(epe2)) - log(det(epe1)))
    lratio = tminusc*(log(det(epe2)) - log(det(epe1)));

    % find marginal probability
    lprob = chis_prb(lratio,neqs*neqs);

out = [i i-1 lratio 1-lprob];
fprintf(1,'nlag = %2d %2d, LR statistic = %16.4f, probability = %6.4g \n',out);

end;
