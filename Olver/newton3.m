function newton3(x0,y0,n)

%  newton3(x0,y0,n)
%
%  Uses Newtons method to solve real system for z^3 = 1
%
%  x0,y0 --  initial guess
%   n    --  # of iterations;  n = 0 gives interactive version
%

clf;
colors = ['kbgrcmy'];
ncolors = size(colors,2);

axis([-1.5 1.5 -1.5 1.5]); axis manual; hold on;
solx = [1 -.5 -.5]; soly = [0 -.8666 .8666];
plot(solx,soly,'rx');

nn = n; if n <= 0 nn = 100; end

v = [x0;y0];
	fprintf('\n  n          point\n\n')

for i=0:nn
   plot(v(1),v(2),[colors(mod(i,ncolors)+1),'o'])
    fprintf(1,'%3d     ',i)
    fprintf(1,'%5.4f     ',v')
    fprintf(1,'\n')
	if n <= 0 disp('Press any key to continue. . .'), pause;end
   x = v(1); y = v(2);
   w = [x^3 - 3*x*y^2 - 1; 3*x^2*y - y^3];
   A = [3*x^2 - 3*y^2, -6*x*y;6*x*y, 3*x^2 - 3*y^2];
   v = v - A\w;
end


