function xlag = sdiff(x,freq)
% PURPOSE: generates a vector or matrix of lags
% -----------------------------------------------
% USAGE:    xsdiff = sdiff(x,freq)
%       or: xsdiff = sdiff(x,cstruc)
% Where:         x = a vector or matrix of length nobs
%             freq = 4 for quarterly, =12 for monthly
%           cstruc = a structure returned by cal()
% -----------------------------------------------
% RETURNS: xsdiff = a seasonlly differenced matrix or vector,
%                   length nobs (first freq values = 0)
% -----------------------------------------------
% SEE ALSO: tdiff() 

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin ~= 2
error('Wrong # of arguments to sdiff');
end;

if isstruct(freq)
freq = freq.freq;
end;

[nobs, nvar] = size(x);

tmp = zeros(freq,nvar);
xlag = zeros(nobs,nvar);
xlag = [tmp
        x(freq+1:nobs,:) - x(1:nobs-freq,:)];
        
