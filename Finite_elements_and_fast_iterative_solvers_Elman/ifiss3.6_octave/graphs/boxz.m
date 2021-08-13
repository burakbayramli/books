%BOXZ outlines L:H cavity domain in monochrome
%   IFISS scriptfile: DJS; 7 August 2010.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.
hold on
plot([0,L],[0,0],'-k');
plot([L,L],[0,H],'-k');
plot([L,0],[H,H],'-k');
plot([0,0],[H,0],'-k');
axis([-0.2,L+.2,-0.2, H+.2])
hold off
