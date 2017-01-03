function y = sdummy(nobs,freq)
% PURPOSE: creates a matrix of seasonal dummy variables
%---------------------------------------------------
% USAGE:      y = sdummy(nobs,freq);
%         or: y = sdummy(nobs,cstruc)
% where:   freq = 4 for quarterly, 12 for monthly
%        cstruc = a structure returned by cal()
%     
%---------------------------------------------------
% RETURNS: 
%        y = an (nobs x freq) matrix with 0's and 1's
%          e.g.,   1 0 0 0  (for freq=4)
%                  0 1 0 0
%                  0 0 1 0
%                  0 0 0 1
%                  1 0 0 0 
% 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin ~= 2
error('Wrong # of arguments to sdummy');
end;

if isstruct(freq)
freq = freq.freq;
end;

z = zeros(nobs,freq);

for i=1:freq:nobs;
z(i:i+freq-1,1:freq) = eye(freq);
end;

y = z(1:nobs,:);


