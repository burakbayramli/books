colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
x = linspace(-pi,pi,m);

f = pi - abs(x);
%f = sign(x);
%f = x -  .5 * pi *(sign(x) - 1);

mx = max(max(f)+1,1); mn = min(min(f)-1,-1);

plot(x,f,'k')
axis([-pi pi mn mx]);
zoom on;


ic = 1;
