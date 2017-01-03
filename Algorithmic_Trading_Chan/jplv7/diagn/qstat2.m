function [qstat,pval] = qstat2(x,p) 
% PURPOSE: computes the Ljung-Box Q test for AR(p) 
%-------------------------------------------------------- 
% USAGE: result = qstat2(x,p) 
% where: x  = a vector (usually regression residuals) 
%        p  = order(s) of AR to be tested (scalar or 
%             row/column vector) 
%-------------------------------------------------------- 
% RETURNS: 
%        qstat    = (vector of) Q(p) test statistic(s),  
%      distributed Chi-squared(p) under H0 
%        pval     = tail probabilit(ies) of the qstat(s) 
%-------------------------------------------------------- 
% REQUIRES: 
%        cols(), rows(), sacf(), seqa(), chis_cdf from LeSage library  

% written by: 
% Kit Baum 
% Dept of Economics % Boston College 
% Chestnut Hill MA 02467 USA % baum@bc.edu 
% 9601 rev 9607  

if (nargin ~= 2)    error('Wrong number of arguments to qstat'); end;  
[n1 n2] = size(p); 
if n1 > n2 p = p'; end;  
c = cols(p); n = rows(x); 
for i=1:c; 
rho = sacf(x,p(i),1); 
rho2 = rho .* rho; 
qstat(i) = n*(n+2)*sumc((rho2 ./ seqa(n-1,-1,p(i)))); 
pval(i) = 1-chis_cdf(qstat(i),p(i)); 
end;   