function  G=numz(b,infoz,stat,varargin)
% PURPOSE: Evaluate numerical derivs in MINZ package
%-----------------------------------------------------------------
% USAGE:  G=numz(b,infoz,stat,varargin)
% Where
%   b:        k-vector of parms
%   infoz:     structure variable with options for MINZ
%   stat:     structure variable with status for MINZ
%   varargin: variable number of arguments needed by function 
%               being differentiated (infoz.func or infoz.momt)
%-----------------------------------------------------------------
%VERSION: 1.1.1 (9/23/00)

% written by:
% Mike Cliff,  Purdue Finance  mcliff@mgmt.purdue.edu
% CREATED: 12/8/98
% MODIFIED: 9/23/00 (1.1.1 fcnchk)

if ~isfield(infoz,'delta')
  dh=.000001;             % Sets precision of Numerical Derivs
else
  dh = infoz.delta;
end

if (strcmp(infoz.call,'ls') | strcmp(infoz.call,'gmm'))
  func = fcnchk(infoz.momt);
%  T = cols(varargin{1})*cols(varargin{3});
else
  func = fcnchk(infoz.func);
%  T = 1;
end;

g = feval(func,b,infoz,stat,varargin{:});
T = rows(g);

k=rows(b);
e=eye(k); b0=b;
G=zeros(T,k);

for i = 1:k
  b = b0 + e(:,i)*dh; gplus=feval(func,b,infoz,stat,varargin{:});
  b = b0 - e(:,i)*dh; gminus=feval(func,b,infoz,stat,varargin{:});
  G(:,i)=(gplus-gminus)/(2*dh);
end

if ~(strcmp(infoz.call,'ls') | strcmp(infoz.call,'gmm'))
  G = G';  
end;
