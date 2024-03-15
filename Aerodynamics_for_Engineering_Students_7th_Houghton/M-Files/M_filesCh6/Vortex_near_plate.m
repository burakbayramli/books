% Computations inspired by Problem 4.9 in text by
% Houghton et al (5e and 6e)
%
%     Potential flow around a flat plate by
% using the 1/4-3/4-chord 'Weissinger' panels. 
% N = number of panels.
%
% Daniel T. Valentine .................. March 2012.
clear;clc
N = 100;
% Geometry of a cylinder
%theta = 0:2*pi/N:2*pi;
%R = 1;
% for j=1:N+1
%     x(j) = R*cos(theta(j));
%     y(j) = R*sin(theta(j));
% end
% VORTEX NEAR PLATE INTERACTION
% Geometry of a flat plate at angle of attack
alpha = 0; % degrees
% x = 0:1/N:1;
for j=1:N+1;
    x(j) =  cos(alpha*pi/180)*(j-1)/N;
    y(j) = -sin(alpha*pi/180)*(j-1)/N;
end
c = x(N+1) - x(1);
dx = c/(N+1);
% Vortex
Gam = 1;
xnu = 0.5; % Above mid-chord location
hv = 5:.5:20;
for ih = 1:length(hv)
    h = hv(ih);
ynu = h; % >> xnu as per Problem 9 in the text.
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
    vcx(j) =  Gam*(ynu-yc(j))/( 2*pi*((xnu-xc(j))^2 + (ynu-yc(j))^2));
    vcy(j) = -Gam*(xnu-xc(j))/( 2*pi*((xnu-xc(j))^2 + (ynu-yc(j))^2 ));
    b(j,1) =  (uinfty+vcx(j))*normx(j) + (0+vcy(j))*normy(j);
end
% Replace last equation by ones and set b = 0; this 
% ensures that the net circulation is zero (works without this!!!!!)
% A(N,:) = ones(1,N);
% b(N,1) = 0.0;
vortex_strength_vector = A\b;
% The -A\b vector is the solution sought
% Computation of the lift
vcc = uinfty + vcx';
dC = -vcc.*vortex_strength_vector;
CL = -2*sum(dC);
%disp(['alpha in degrees = ',num2str(alpha),',     CL = ',num2str(CL)])
CLs(ih) = CL;
CLdtv(ih) = 2*(uinfty + Gam/2/pi/h)*Gam*c^2/8/h/h;
end
%
 plot(hv,CLs,'k',hv,CLdtv,'or')