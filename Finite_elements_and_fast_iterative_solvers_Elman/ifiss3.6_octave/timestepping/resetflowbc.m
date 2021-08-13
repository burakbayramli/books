function unew = resetflowbc(uold,xy,bound,t)
%RESETFLOWBC resets boundary values in unsteady flow 
%   unew = resetflowbc(uold,xy,bound,t);
%   input
%          uold       velocity  vector
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          t          current time
%   output
%          unew        updated velocity vector  
%
%   calls function specific_flow
%   IFISS function: DJS; 22 November 2009.
% Copyright (c) 2009 D.J. Silvester, D.F. Griffiths, M. Schneider  
nu = length(uold);
nvtx = nu/2; nbd=length(bound);
fx=uold(1:nvtx); fy=uold(nvtx+1:nu); 

%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
[bcx,bcy]=specific_flow(xbd,ybd);
% smoothly grow the hot wall profile
bcx=bcx*(1-exp(-10*t)); bcy=bcy*(1-exp(-10*t));
fx(bound)=bcx;  fy(bound)=bcy; 
unew=[fx;fy]; 
return
