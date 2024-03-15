%
%     Potential flow around a camber plate by
% using the 1/4-3/4-chord 'Weissinger' panels. 
% N = number of panels.
%
% Daniel T. Valentine ............. 2009/2016.
clear;clc
N = 100;
% Geometry of a camber 
x = 0:1/N:1; 
fc = 0.052; % Maximum camber to chord ratio
% Parabolic camber
% for j=1:N+1; y(j)=fc*x(j)*(1-x(j));end 
% Cubic camber + angle of attack loading
alpha = 3; % angle of attack
% 
for j=1:N+1  
    y(j) = fc*x(j)*(x(j)-1)*(x(j)-2) ...  % camber loading 
        -sin(alpha*pi/180)*(j-1)/N;     % angle of attack loading
end 
%
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
    b(j,1) =  uinfty*normx(j) + 0*normy(j);
end
% Replace last equation by ones and set b = 0; this 
% ensures that the net circulation is zero (works without this!!!!!)
% A(N,:) = ones(1,N);
% b(N,1) = 0.0;
vortex_strength_vector = -A\b;
% The -A\b vector is the solution sought
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
figure(1)
plot(xvort,yvort,'k');%'x'); hold on
plot(xc,yc,'k');%'o')
hold off
figure(2)
plot(xvort,-vortex_strength_vector)
title('Pressure jump across surface of flat plat')
xlabel(' x');ylabel('\Deltap')
%
% Computation of the lift
%
C = - sum(vortex_strength_vector);
xc = xvort';
CMLE = 2*sum(xc.*vortex_strength_vector);
CL = 2*C; disp('The lift and moment coefficients are as follows: ')
disp([' CL = ',num2str(CL)])
CMqt = (CMLE/CL + 0.25)*CL;
disp([' CM about the quarter chord = ',num2str(CMqt)])
disp(' Note that a negative moment is pitch down. ')
%