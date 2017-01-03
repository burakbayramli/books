function demoCubicPoly
%DEMOCUBICPOLY demo of fitting a cubic polynomial
% Linear Parameter Regression : example using Cubic Polynomial
figure
data=[
   20.0 16.0 19.8 18.4 17.1 15.5 14.7 17.1 15.4 16.2 15.0 17.2 16.9 17.0 14.4 ;
   88.6 71.6 93.3 84.3 80.6 75.2 69.7 82.0 69.4 83.3 79.6 82.6 80.6 83.5 76.3];
x = data(2,:); y = data(1,:); plot(x,y,'x')
w=LPMregtrain(x,y,'LPMcubicpoly',10e-2);
xp = 65:100; yp = w'*LPMcubicpoly(xp); hold on; plot(xp,yp)