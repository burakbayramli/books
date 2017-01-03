colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
x = linspace(0,pi,m);

f = ones(1,m);
%f = x;
mx = max(max(f)+1,1); mn = min(min(f)-1,-1);

plot(x,f,'k')
axis([0 pi mn mx]);
zoom on;


ic = 1;
