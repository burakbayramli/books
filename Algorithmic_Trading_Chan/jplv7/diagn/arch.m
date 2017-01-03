function [archstat,pval] = arch(x,p) 
% PURPOSE: computes a test for ARCH(p) 
% -------------------------------------------------------- 
% USAGE: [archstat,pval] = arch(x,p) 
%  where: x  = a vector (usually regression residuals) 
%         p  = order(s) of ARCH to be tested (scalar or 
%              row/column vector) 
% -------------------------------------------------------- 
% RETURNS: 
%         archstat = ARCH(p) test statistic(s), distributed 
%                    Chi-squared(p) under H0 
%         pval     = tail probabilit(ies) 
% -------------------------------------------------------- 
% REQUIRES: 
%         cols(), rows(), mlag(), chis_cdf() from LeSage library 
% -------------------------------------------------------- 
% REFERENCES: 
% Engle, Robert (1982), "Autoregressive Conditional Heteroskedasticity with Estimates of 
%     the Variance of United Kingdom Inflation", Econometrica, vol. 50, pp. 987-1007 
% Ljung, G.M. & G.E.P. Box (1978), "On a Measure of Lack of Fit in Time Series Models", 
%     Biometrika, vol. 65, no. 2, pp. 297-303 
% McLeod, A.I. & W.K. Li (1983), "Diagnostic Checking ARMA Time Series Models Using 
%  Squared-Residual Autocorrelations", Journal of Time Series Analysis, vol. 4, no. 4, 
%  pp. 269-273 
%--------------------------------------------------------  

%   written by: 
%   Kit Baum 
%   Dept of Economics 
%   Boston College 
%   Chestnut Hill MA 02467 USA 
%   baum@bc.edu 
%   9601 rev 9607  

 if (nargin ~= 2)    error('Wrong number of arguments to arch'); end;  
 [n1 n2] = size(p); 
 if n1 > n2 p = p'; end;  
 c = cols(p); n = rows(x); x2 = x .* x; 
 for i=1:c; 
 lx2 = mlag(x2,p(i)); 
 lx2(1:p(i),:)=[]; 
 lxx = [ones(n-p(i),1) lx2]; 
 olsr = ols(x2(p(i)+1:n),lxx); 
 archstat(i) = (n-p(i))*olsr.rsqr;
 pval(i) = 1-chis_cdf(archstat(i),p(i)); 
 end;  
 % end of function  