function plt_cus(results)
% PURPOSE: plots cusum squared tests
%          using results structure from cusums
%---------------------------------------------------
% USAGE: plt_cus(results)
% where: results = a structure returned by cusums
% --------------------------------------------------
% RETURNS:
%        nothing, simply plots the cusums plus confidence
%                 intervals
% NOTE: truncates first k+1 observations                
% --------------------------------------------------
% SEE ALSO: dfbeta, plt_dff, bkw, rdiag, diagnose
%---------------------------------------------------
% REFERENCES: Brown, Durbin, Evans 1975 J. Royal Statistical
% Society, 'Techniques for testing the constancy of regression
% relationships over time'', Series B, pp. 149-192.

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


% skip k+2 initial observations

k = results.nvar;
[n junk] = size(results.cusums);

csum1 = trimr(results.cusums,k+1,0);
upper95 = trimr(results.upper95,k+1,0);
lower95 = trimr(results.lower95,k+1,0);

tt=k+2:n;
plot(tt,csum1,tt,upper95,'v',tt,lower95,'^');
title('Cusums with 95 percent intervals');
legend('cusums','upper95','lower95');
xlabel('Observations');
