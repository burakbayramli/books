function ch3ex4
% y(1) = y(x), y(2) = z(x), y(3) = y'(x), y(4) = z'(x)
global d
hold on
for i = 1:3
	D = 4*i;
    d = 1/D;
	if i == 1
        P = [-1; 0.5];  % Guess parameters P = [b; c]:
        solinit = bvpinit(linspace(d,D,5),[d; 0; 0; 0],P);
	else
		solinit = bvpinit(sol,[d,D]);
	end
	sol = bvp4c(@odes,@bcs,solinit);
	plot(sol.x,sol.y(1:2,:),sol.x(end),sol.y(1:2,end),'ro',...
         sol.x(1),sol.y(1:2,1),'ko');
    legend('y(x)','z(x)',0);
	axis([0 12 0 1]);
	drawnow
end
hold off

%===========================================================
function dydx = odes(x,y,P)
dydx = [ y(3); y(4); 2*(y(3) - y(2))/(3*y(1)); -y(1)*y(4) ];
 
function res = bcs(ya,yb,P)
global d
res = zeros(6,1);
res(1:4) = ya - series(d,P);
res(5:6) = [ yb(3); (yb(4) - (- yb(1)*yb(2))) ];

function y = series(x,P)
b = P(1);
c = P(2);
yx  = x + b*x^(5/3) - c*x^2 - (5/7)*b^2*x^(7/3) ...
      + (7/6)*b*c*x^(8/3);
ypx = 1 + (5/3)*b*x^(2/3) - 2*c*x - (5/3)*b^2*x^(4/3)...
      + (28/9)*b*c*x^(5/3);
zx  = 1 + c*x - (1/6)*c*x^3 - (9/88)*b*c*x^(11/3);
zpx = c - (1/2)*c*x^2 - (3/8)*b*c*x^(8/3);
y = [ yx; zx; ypx; zpx];