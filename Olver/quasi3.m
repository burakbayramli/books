function quasi3(a3,f3,a2,f2,a1,f1,b)
%
%  quasi3(a3,f3,a2,f2,a1,f1,b)
%
% Plots the quasi-periodic function
%   a1 sin(f1 * x)  + a2 sin(f2 * x)  + a3 sin(f3 * x)
%  from x = 0 to b
%
%  all arguments are optional
%
%   See also QUASI

n = 1000;

if nargin < 7,  b = 50; end 
if nargin < 6,  a1 = 1; f1 = 1; end 
if nargin < 4, a2 = .5; f2 = pi; end 
if nargin < 2,  a3 = .4; f3 = sqrt(2); end 

%f2 = 3/4; b = 100;

dy = a1 + a2 + a3 + .5;
axl = [0 b -dy dy];

if f1 == 1, s1 = ''; else s1 = num2str(f1); end
if f2 == 1, s2 = ''; else s2 = num2str(f2); end
if f3 == 1, s3 = ''; else s3 = num2str(f3); end
if a1 == 1, z1 = ''; else z1 = num2str(a1); end
if a2 == 1, z2 = ''; else z2 = num2str(a2); end
if a3 == 1, z3 = ''; else z3 = num2str(a3); end

x = linspace(0,b,n);

y1 = a1 * sin(f1*x);
y2 = a2 * sin(f2*x);
y3 = a3 * sin(f3*x);

z = y1 + y2 + y3;

subplot('position',[.07 .8 .9 .15]); 
	plot(x,y1); axis(axl); title([z1,' sin ',s1,' x'])
subplot('position',[.07 .55 .9 .15]); 
	plot(x,y2); axis(axl); title([z2,' sin ',s2,' x'])
subplot('position',[.07 .3 .9 .15]); 
	plot(x,y3); axis(axl); title([z3,' sin ',s3,' x'])
subplot('position',[.07 .05 .9 .15]); 
	plot(x,z); axis(axl);
	title([z1,' sin ',s1,' x  +  ',z2,' sin ',s2,' x  ',z3,' sin ',s3,' x'])
