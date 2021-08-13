%ELLX outlines L-shaped domain
%   IFISS scriptfile: DJS; 28 February 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
hold on
plot([0,0],[0,-1],'-k');
plot([0,1],[-1,-1],'-k');
plot([1,1],[-1,1],'-k');
plot([1,-1],[1,1],'-k');
plot([-1,-1],[1,0],'-k');
plot([-1,0],[0,0],'-k');
hold off
