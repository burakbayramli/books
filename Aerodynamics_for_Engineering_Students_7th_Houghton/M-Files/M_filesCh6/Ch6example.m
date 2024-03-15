%
% Camber for k=ko=constant
clear;clc
c=1; U=1; ko = .1;
N=600;
C = c*ko/2/pi/U; % C = CL/2;
x1 = .0001; x2 = 1-.0001; DX = x2-x1;
x = x1:DX/N:x2;
for nc = 1:N+1
y(nc) = -C*( (1-x(nc))*log(1-x(nc)) + x(nc)*log(x(nc)) );
end
uinfty = U;
%
% Location of point vortices and
% collocation points:
for j=1:N
% Point vortices locations
xvort(j) = x(j) + 0.25*(x(j+1)-x(j));
yvort(j) = y(j) + 0.25*(y(j+1)-y(j));
dsP(j) = sqrt((x(j+1)-x(j))^2+(y(j+1)-y(j))^2);
% Collocation points location
xc(j) = x(j) + 0.75*(x(j+1)-x(j));
yc(j) = y(j) + 0.75*(y(j+1)-y(j));
% Normal to the panel
normx(j) = (y(j+1)-y(j));
normy(j) = -(x(j+1)-x(j));
lengthP = sqrt(normx(j)^2+normy(j)^2);
normx(j) = normx(j)/lengthP;
normy(j) = normy(j)/lengthP;
end
% Determination of the velocity at the
% collocation points due to unit vortices:
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
b(j,1) = uinfty*normx(j) + 0*normy(j);
end
%
vortex_strength_vector = -A\b;
%
for j=1:N
u(j) = uinfty; v(j) = 0;
for m=1:N
dx = xc(j) - xvort(m);
dy = yc(j) - yvort(m);
r = sqrt(dx^2+dy^2);
vx = -1/(2*pi*r)*dy/r;
vy = 1/(2*pi*r)*dx/r;
u(j) = u(j) + vortex_strength_vector(m)*vx;
v(j) = v(j) + vortex_strength_vector(m)*vy;
end
end
delP = -vortex_strength_vector./dsP';
plot(xvort,delP,'r')
title('Pressure jump across surface of thin plate')
xlabel(' x');ylabel('\Deltap')
%
% Computation of the lift
C = - sum(vortex_strength_vector); xc = xvort';
CMLE = 2*sum(xc.*vortex_strength_vector);
CL = 2*C; disp('The lift and moment coefficients are as follows: ')
disp([' CL = ',num2str(CL)])
CMqt = (CMLE/CL + 0.25)*CL;
disp([' CM about the quarter chord = ',num2str(CMqt)])
disp(' Note that a negative moment is pitch down. ')
%