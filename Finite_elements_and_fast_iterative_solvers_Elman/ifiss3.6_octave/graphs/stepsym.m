%STEPSYM outlines symmetric step domain
%   IFISS scriptfile: DJS; 19 November 2014.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
if exist('x','var')==0,
   error('You need to set up a step-shaped domain first!'), 
end
varsteplen = max(x);
hold on
plot([-1,0],[-0.5,-0.5],'-k');
plot([0,0], [-0.5,-1],'-k');
plot([0,varsteplen],[-1,-1],'-k');
plot([varsteplen,varsteplen],[-1,1],'-k');
plot([varsteplen,0],[1,1],'-k');
plot([0,0], [1,0.5],'-k');
plot([-1,0],[0.5,0.5],'-k');
plot([-1,-1],[-0.5,0.5],'-k');
hold off
