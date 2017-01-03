function p = robot(l,a,m,b,x,y,n)

%  robot(l,a,m,b,x,y,n)
%
%   Uses Newton's method to position two robot arms
%
%   l = length of first arm
%   a = initial angle of first arm
%   m = length of second arm
%   b = initial angle of second arm
%   x,y = final position of end of second arm
%   n = # of iterations;  n = 0 gives interactive version
%   

clf;
colors = ['kbgrcmy'];
ncolors = size(colors,2);

axis([-l-m l+m -l-m l+m]); axis manual

nn = n; if n <= 0 nn = 100; end
z = [x;y];
p = [a;b];
	fprintf('\n  n          angles                 first node              second node\n\n')

for i=0:nn
	q1 = [l*cos(p(1));l*sin(p(1))]; q2 = [m*cos(p(2));m*sin(p(2))]; q = q1+q2;
    line([0 q1(1) q(1)],[0 q1(2) q(2)],'color',colors(mod(i,ncolors)+1));
    fprintf(1,'%3d     ',i)
    fprintf(1,'%5.4f     ',p')
    fprintf(1,'%5.4f     ',q1')
    fprintf(1,'%5.4f     ',q')
    fprintf(1,'\n')
	if n <= 0 disp('Press any key to continue. . .'), pause;end
	p = p - [-l*sin(p(1)),-m*sin(p(2));l*cos(p(1)),m*cos(p(2))]\(q - z);
end
