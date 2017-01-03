function quasi(a2,f2,a1,f1,b)
%
%  quasi(a2,f2,a1,f1,b)
%
% Plots the quasi-periodic function
%   a1 sin(f1 * x)  + a2 sin(f2 * x)
%  from x = 0 to b
%
%  all arguments are optional
%
%   See also QUASI3

n = 1000;

if nargin < 5,  b = 50; end 
if nargin < 4,  a1 = 1; f1 = 1; end 
if nargin < 2, a2 = .5; f2 = pi; end 

dy = a1 + a2 + .5;
axl = [0 b -dy dy];

%f2 = 3/4; b = 100;

if f1 == 1, s1 = ''; else s1 = num2str(f1); end
if f2 == 1, s2 = ''; else s2 = num2str(f2); end
if a1 == 1, z1 = ''; else z1 = num2str(a1); end
if a2 == 1, z2 = ''; else z2 = num2str(a2); end

x = linspace(0,b,n);

y1 = a1 * sin(f1*x);
y2 = a2 * sin(f2*x);

z = y1 + y2;

subplot(3,1,1); plot(x,y1); axis(axl); title([z1,' sin ',s1,' x'])
subplot(3,1,2); plot(x,y2); axis(axl); title([z2,' sin ',s2,' x'])
subplot(3,1,3); plot(x,z); axis(axl);
 title([z1,' sin ',s1,' x  +  ',z2,' sin ',s2,' x'])
