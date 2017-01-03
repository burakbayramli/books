
%5.4  rungecode.m

function runge
% Illustrates the Runge phenomenon of oscillations in polynomial
% interpolation. The function f(x)=1/(1+25x^2) is fitted with 
% interpolating polynomials of degree 6, 8, and 10 on [-1,1], 
% all of which show oscillations near the ends of the interval.

% Create the data points for plotting f(x)
xx=linspace(-1,1,150);   yy=1./(1+25*xx.^2);

% 6 data points needed to find the 5-th degree interpolating polynomial p6
x5=linspace(-1,1,6);    y5=1./(1+25*x5.^2);     p5=polyfit(x5,y5,5);

% 9 data points needed to find the 8-th degree interpolating polynomial p8
% x8=linspace(-1,1,9);    y8=1./(1+25*x8.^2);     p8=polyfit(x8,y8,8);

% 11 data points needed to find the 10-th degree interpolating polynomial p10
x10=linspace(-1,1,11);  y10=1./(1+25*x10.^2);   p10=polyfit(x10,y10,10);

% plot f(x)
plot(xx,yy,'-b','LineWidth',3), hold on

x = -1:0.2:1; y = 1./(1+25*x.^2);
%plot interpolations
%change x5, x8, x10 below to xx for plotting the interpolants at all points xx
plot(xx,polyval(p5,xx),'-k','LineWidth',2)
% plot(xx,polyval(p8,xx),'-go','LineWidth',2)
plot(xx,polyval(p10,xx),'-k','LineWidth',2)

% legend('f=1/(1+25x^2)','5th degree polynomial','10th degree polynomial')
grid on
axis([-1 1 -0.3 1.2]);
hold on;
plot(x,y,'o','LineWidth',2,'MarkerSize',8)
end
