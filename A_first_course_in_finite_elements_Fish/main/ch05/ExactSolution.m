% plots the exact stress 
function ExactSolution
include_flags;

% divide the problem domain into two regions
xa = 2:0.01:5; 
xb = 5:0.01:6;

subplot(2,1,1);
% exact displacement for xa
c1 = 72;  c2 = 1 - (c1/16)*log(2);
u1 = -.5*xa + (c1/16)*log(xa) + c2;
% exact displacement for xb
c3 = 48;  c4 = log(5)/16*(c1-c3) + c2;
u2 = -.5*xb + (c3/16)*log(xb) + c4;
% plot displacement
h=plot([xa xb],[u1 u2], '--r' );      
legend(h,'exact');

subplot(2,1,2);
% exact stress for xa
ya = (36-4*xa)./xa;   
% exact stress for xb
yb = (24-4*xb)./xb;   
% plot stress
plot([xa xb],[ya yb], '--r' );      
