function f = lsfunc(b,infoz,stat,varargin)
% PURPOSE: Evaluate least squares function m(b)'Wm(b) at b
% -------------------------------------------------------------
% USAGE: function f = lsfunc(b,infoz,stat,varargin)
% Where: b          = parameter vector fed to func
%        infoz       = infoz structure (see MINECON for details)
%        stat       = status structure (see MINECON for details)
%        varargin   = arguments used by 
% -------------------------------------------------------------
% RETURNS: f    = function value evaluated at b
% -------------------------------------------------------------
% VERSION: 1.1.1 (1/24/99)

% written by:
% Mike Cliff,  Purdue Finance   mcliff@mgmt.purdue.edu
% CREATED:  12/10/98
% UPDATED:  1/24/99 (1.1)
%           9/23/00 (1.1.1 fcnchk)  
  
lvar = length(varargin);
if strcmp(infoz.call,'gmm')
  W=varargin{lvar}; wdum = 1;
else
  W = 1;  wdum=0;
end;
  
momt = fcnchk(infoz.momt);
m = feval(momt,b,infoz,stat,varargin{1:lvar-wdum});

f = m'*W*m;
