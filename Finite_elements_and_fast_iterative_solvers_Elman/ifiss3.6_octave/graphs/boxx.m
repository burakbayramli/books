%BOXX outlines L:H cavity domain in hot/cold 
%   IFISS scriptfile: DJS; 14 December 2009.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.
if exist('hty','var')==0, hty=2; end  %% default: right/left 
hold on
if hty==2,
plot([0,L],[0,0],'-k');
plot([L,L],[0,H],'-b');
plot([L,0],[H,H],'-k');
plot([0,0],[H,0],'-r');
else
plot([0,L],[0,0],'-r');
plot([L,L],[0,H],'-k');
plot([L,0],[H,H],'-b');
plot([0,0],[H,0],'-k');
end
axis([-0.2,L+.2,-0.2, H+.2])
hold off
