function grwth = growthr(x,freq)
% PURPOSE: converts the matrix x to annual growth rates
%---------------------------------------------------
% USAGE:     result = growthr(x,freq)
%        or: result = growthr(x,cstruc)
% where:     x = a matrix or vector
%         freq = 12 for monthly, 4 for quarterly, 1 for annual
%       cstruc = a structure returned by cal()
%---------------------------------------------------
% RETURNS:
%        results = annual % change in x element vectors
%                  e.g. {(x(t) - x(t-freq))/x(t-freq)}*100
% --------------------------------------------------
% SEE ALSO: tdiff
%---------------------------------------------------

if nargin ~=2
error('growthr: wrong # of input arguments');
end;

if isstruct(freq)
freq = freq.freq;
end;

[nobs nvar] = size(x);

xmat = x(freq+1:nobs,:);
xlag = x(1:nobs-freq,:);

[t1 t2] = find(xlag ~= 0);
if length(unique(t1) == nobs) & length(unique(t2) == nvar)
 xchg = (xmat - xlag)./xlag;
else  
 [t1 t2] = find(xlag == 0);
 xlag(t1,t2) = 0.01;
xchg = (xmat - xlag)./xlag;
end;

% fill-in first freq obs with zeros

grwth = [zeros(freq,nvar)
         xchg*100];