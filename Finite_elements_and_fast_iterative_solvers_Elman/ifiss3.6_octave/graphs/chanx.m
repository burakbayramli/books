%CHANX outlines channel domain
%   IFISS scriptfile: DJS; 27 July 2015.
% Copyright (c) 2015 D.J. Silvester, H.C. Elman, A. Ramage
if exist('x','var')==0,
   error('You need to set up a channel domain first!'),
end
varsteplen = max(x);
hold on
plot([-1,varsteplen],[-1,-1],'-k');
plot([varsteplen,varsteplen],[-1,1],'-b');
plot([varsteplen,-1],[1,1],'-k');
plot([-1,-1],[1,-1],'-k');
hold off
