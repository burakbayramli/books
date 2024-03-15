% PROBLEM 9 in Chapter 4 of the text.
% Note that the increment of lift, according to the text, 
% is approximately equal 4*Gamv/(3*pi*h*h), in which the 
% formula for the induced velocity given in the problem 
% statement defined the circulation, Gamv = Gam/pi, 
% where Gam is the circulation of the vortex disturbance 
% assumed in the computations below. To compare with the 
% results reported by Valentine in the solutions manual, 
% this formula had to be scaled by a factor of 4; a factor 
% of 2 is needed to change L to CL. 
%     A better approximate formula (as derived by Valentine 
% in the solutions manual) is: 
%
%   L = rho * ( uinfity + Gam/2/pi/h ) * Gam*c*c/8/h/h
%
% Potential flow around a flat plate by using the 1/4-3/4-chord 
% 'Weissinger' panels. N = number of panels.
%
% Daniel T. Valentine ............................. March 2012.
clear;clc
N = 100;
%
% VORTEX NEAR PLATE INTERACTION
% Geometry of a flat plate at angle of attack
  alpha = 0; % degrees
for j=1:N+1;
x(j) =  cos(alpha*pi/180)*(j-1)/N;
y(j) = -sin(alpha*pi/180)*(j-1)/N;
end
c = sqrt( (x(N+1)- x(1))^2 + (y(N+1)- y(1))^2 ); % Chord length
dx = c/(N+1);      % Panel width.
rho = 1; % Mass density
% Vortex disturbance
Gam = 1;
Gamv = Gam/pi;
xnu = 0.5; % Above mid-chord location
hv = 5:.5:20;
for ih = 1:length(hv)
    h = hv(ih);
xnu = 0.5 + h*sin(alpha*pi/180);
ynu = h*cos(alpha*pi/180); % >> xnu as per Problem 9 in the text.
% Free stream
uinfty = 1;
%
% Location of point vortices and
% collocation points
%
for j=1:N
% Point vortices locations
xvort(j) = x(j) + 0.25*(x(j+1)-x(j));
yvort(j) = y(j) + 0.25*(y(j+1)-y(j));
% Collocation points location
xc(j) = x(j) + 0.75*(x(j+1)-x(j));
yc(j) = y(j) + 0.75*(y(j+1)-y(j));
% Normal to the panel
normx(j) = (y(j+1)-y(j));
normy(j) = -(x(j+1)-x(j));
lengthP = sqrt(normx(j)^2+normy(j)^2);
ds(j) = lengthP;
normx(j) = normx(j)/lengthP;
normy(j) = normy(j)/lengthP;
end
% Determination of the velocity at the
% collocation points due to unit vortices
% distance from the vortices to the collocation
% POINT
for j=1:N
for k=1:N
dx = xc(j) - xvort(k);
dy = yc(j) - yvort(k);
r = sqrt(dx^2+dy^2);
vx = -1/(2*pi*r)*dy/r;
vy = 1/(2*pi*r)*dx/r;
norm_velocity = vx*normx(j) + vy*normy(j);
A(j,k) = norm_velocity;
end
vcx(j) =  Gam*(ynu-yc(j))/( 2*pi*((xnu-xc(j))^2 + (ynu-yc(j))^2) );
vcy(j) = -Gam*(xnu-xc(j))/( 2*pi*((xnu-xc(j))^2 + (ynu-yc(j))^2) );
b(j,1) = (uinfty+vcx(j))*normx(j) + (0+vcy(j))*normy(j);
end
% Replace last equation by ones and set b = 0; this
% ensures that the net circulation is zero (works without this!!!!!)
% A(N,:) = ones(1,N);
% b(N,1) = 0.0;
vortex_strength_vector = -A\b;
% The -A\b vector is the solution sought
%
% Computation of the lift
vcc = uinfty + vcx';
dC = vcc.*vortex_strength_vector;
CL = -2*sum(dC);
% disp(['alpha in degrees = ',num2str(alpha),', h = ',num2str(h),', CL = ',num2str(CL)])
CLhv(ih) = CL;
CLapp(ih) = 2*(4*Gamv/3/pi/h/h)+ 2*pi*sin(alpha*pi/180);
CLdtv(ih) = 2*(uinfty + Gam/2/pi/h)*Gam*c^2/8/h/h + 2*pi*sin(alpha*pi/180);
end
%plot(hv,CLhv,'k',hv,CLdtv,'ok')
plot(hv,CLhv,'k',hv,CLapp,'+k',hv,CLdtv,'ok')
xlabel('h'),ylabel('C_L')
%title('Solution to question raised in Problem 4.9')
title('Addendum to solution to question raised in Problem 4.9')
legend('Panel method, N=100 panels',...
   'Adjusted formula in problem statement', 'Large h DTV theory')
%legend('Panel method, N=100 panels','Large h DTV theory')