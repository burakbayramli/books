function  G=numzz(b,infoz,stat,varargin)
% PURPOSE: Evaluate numerical derivs in MINZ package
%-----------------------------------------------------------------
% USAGE:  G=numzz(b,infoz,stat,varargin)
% Where
%   b:        k-vector of parms
%   infoz:    structure variable with options for MINZ
%   stat:     structure variable with status for MINZ
%   varargin: variable number of arguments needed by function 
%               being differentiated (infoz.func or infoz.momt)
%-----------------------------------------------------------------

% written by:
% Mike Cliff,  UNC Finance  mcliff@unc.edu
% CREATED: 12/8/98

if ~isfield(infoz,'delta')
  dh=.000001;             % Sets precision of Numerical Derivs
else
  dh = infoz.delta;
end

if (strcmp(infoz.call,'ls') | strcmp(infoz.call,'gmm'))
  func = fcnchk(infoz.momt,length(varargin)+3);
  T = cols(varargin{1})*cols(varargin{3});
else
  func = fcnchk(infoz.func,length(varargin)+3);
  T = 1;
end;

k=rows(b);
e=eye(k); b0=b;
G=zeros(T,k);
for i = 1:k
  if strcmp(infoz.call,'other');
  b = b0 + e(:,i)*dh; gplus=feval(func,b,varargin{:});
  b = b0 - e(:,i)*dh; gminus=feval(func,b,varargin{:});
  else,
  b = b0 + e(:,i)*dh; gplus=feval(func,b,infoz,stat,varargin{:});
  b = b0 - e(:,i)*dh; gminus=feval(func,b,infoz,stat,varargin{:});
  end;
  G(:,i)=(gplus-gminus)/(2*dh);
end

if ~(strcmp(infoz.call,'ls') | strcmp(infoz.call,'gmm'))
  G = G';  
end;
