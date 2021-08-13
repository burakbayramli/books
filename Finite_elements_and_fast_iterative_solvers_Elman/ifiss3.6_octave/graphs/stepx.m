%STEPX outlines step domain
%   IFISS scriptfile: DJS; 28 February 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
if exist('x','var')==0,
   error('You need to set up a step-shaped domain first!'), 
end
varsteplen = max(x);
hold on
plot([-1,0],[0,0],'-k');
plot([0,0],[0,-1],'-k');
plot([0,varsteplen],[-1,-1],'-k');
plot([varsteplen,varsteplen],[-1,1],'-k');
plot([varsteplen,-1],[1,1],'-k');
plot([-1,-1],[1,0],'-k');
hold off
