function G = lsgrad(b,infoz,stat,varargin)
% PURPOSE: Evaluate M(b)'Wm(b) Gradient of objective function 
% -------------------------------------------------------------
% USAGE: function G = lsgrad(b,infoz,stat,varargin)
% Where: b          = parameter vector fed to func
%        infoz       = infoz structure (see MINZ for details)
%        stat       = status structure (see MINZ for details)
%        varargin   = arguments used by 
% -------------------------------------------------------------
% RETURNS: G    = Gradient evaluated at b
% -------------------------------------------------------------
% NOTES:  Calls the function for e (infoz.momt) and Jacobian
%          E (infoz.jake)
% -------------------------------------------------------------
% VERSION: 1.2.1 (9/23/00)

% written by:
% Mike Cliff,  Purdue Finance,   mcliff@mgmt.purdue.edu
% CREATED:  12/10/98
% UPDATED:  1/24/99
%           7/21/00 (1.2   Added 2* back into G)  
%           9/23/00 (1.2.1 fcnchk)  

lvar = length(varargin);
if strcmp(infoz.call,'gmm')
  W=varargin{lvar};   wdum = 1;
else
  W = 1; wdum=0;
end;

momt = fcnchk(infoz.momt);
jake = fcnchk(infoz.jake);
m = feval(momt,b,infoz,stat,varargin{1:lvar-wdum});
M = feval(jake,b,infoz,stat,varargin{1:lvar-wdum});

G = 2*M'*W*m;