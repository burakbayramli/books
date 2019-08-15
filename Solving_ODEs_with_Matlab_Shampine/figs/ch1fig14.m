% Direction field and solutions for illustrating well-posed.
% The equation is y' = cos(t)y with solutions y(t) = exp(sin(t)).
% Set up grid of points for arrows:
tpts = linspace(0,12,20);
ypts = linspace(-6,6,20);
[T,Y] = meshgrid(tpts,ypts);
% Evaluate the slopes and then normalize the vector (1,S) so that
% it has unit length.
S = cos(T).*Y;
L = sqrt(1 + S.^2);
NL = 1./L;
NS = S./L;
% The arrows are long, so shorten them by half.
quiver(T,Y,NL,NS,1/2)
axis equal tight
% Now plot some solution curves.
mtpts = linspace(tpts(1),tpts(end)+1);
hold on
for ic = [-2 -1 -0.5 0 +0.5 +1 +2]
%	plot(mtpts,ic*exp(sin(mtpts)),'r');
    plot(mtpts,ic*exp(sin(mtpts)),'k');
end
hold off
% print -depsc ch1fig4