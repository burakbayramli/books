function rdiag(y,x)
% PURPOSE: residual analysis plots
%          normal plot, I-chart, histogram, resids vs. yhat
%---------------------------------------------------
% USAGE: rdiag(y,x)
% where: y = dependent variable vector   (from a regression model)
%        x = independent variable matrix (from a regression model)
%---------------------------------------------------
% RETURNS:
%        nothing, produces 4 plots 
% --------------------------------------------------
% SEE ALSO: dfbeta, bkw, diagnose
%---------------------------------------------------
% REFERENCES: Belsley, Kuh, Welsch, 1980 Regression Diagnostics

% M J Chlond - Apr 96
% m.chlond@uclan.ac.uk
% modified by JP LeSage to match
% the format of the econometrics toolbox

result = ols(y,x);

minr = min(result.resid);
maxr = max(result.resid);
minf = min(result.yhat);
maxf = max(result.yhat);
[h,v]= hist(result.resid);
maxh = max(h);
n = length(y);
i = 1:n;
resids = sort(result.resid);

% Montgomery & Peck (p70)
%ns = invcdf((i-1/2)/n,1);

% Myers (p62) - similar to MINITAB
tmp = (i-.375)/(n+.25);
tmp = tmp';
ns = norm_inv(tmp);

minn = min(ns);
maxn = max(ns);
 
subplot(221),plot(ns,resids,'.')
axis([minn maxn minr maxr])
title('Normal Plot of Residuals')
xlabel('Normal Score')
ylabel('Residual')

subplot(222),plot(i,result.resid,i,zeros(1,n))
axis([0 n minr maxr])
title('I Chart of Residuals')
xlabel('Observation Number')
ylabel('Residual')

subplot(223),hist(result.resid)
axis([minr maxr 0 maxh])
title('Histogram of Residuals')
xlabel('Residual')
ylabel('Frequency')

subplot(224),plot(result.yhat,result.resid,'.')
axis([minf maxf minr maxr])
title('Residuals vs. Fits')
xlabel('Fit')
ylabel('Residual')

