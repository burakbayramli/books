%
%     Potential flow around a camber plate by using the 
% 1/4-3/4-chord 'Weissinger' panels. N = number of panels.
% 
% Daniel T. Valentine ......................... 2009/2016.
clear;clc
    N = 200;
x = 0:1/N:1; 
%
% % Problem 1: Logarithmic camber
% CL = 1;
% for j=1:N+1  
%     if j==1 
%         y(j) = 0; 
%     elseif j==N+1
%         y(j) = -(CL/4/pi);
%     else
%  %y(j)= CL*(-(1 - x(j))^2*log(1-x(j)) + x(j)*(x(j)-2)*log(x(j)))/2/pi;
%  % NOTE: The formula below was found by integrating the formula for  
%  % the slope by using MATLAB symbolics toolbox: 
%  y(j)=CL*(1/2*log(1/x(j))-1/2*log(1/x(j)-1)+log(1/x(j)-1)*x(j) ...
%      -1/2*x(j)-1/2*log(1/x(j)-1)*x(j)^2)/2/pi;
%     end
% end 
% % SOLUTION: The lift and moment coefficients are 
% % CL = 0.99813
% % CM about the quarter chord = -0.083964
% 
% % Problem 2: Cubic camber + angle of attack loading
% alpha = 3; % angle of attack
% fc = 0.052; % Maximum camber to chord ratio
% for j=1:N+1  
%      y(j) = fc*x(j)*(x(j)-1)*(x(j)-2) ...  % camber loading 
%            -sin(alpha*pi/180)*(j-1)/N;     % angle of attack loading 
% end 
% % SOLUTION: The lift and moment coefficients are
% % CL = 0.53162
% % CM about the quarter chord = -0.046093
%
% % Problem 3: NACA 4-digit airfoil camber line
% % NACA 8210, where the t = 10/100 = 0.1, c=1 and 
% % t/c is assumed to be zero:
% p = 2/10; 
% m = 8/100; 
% for j=1:N+1  
%     if x(j) <= p
%         y(j) = m*(2*p*x(j) - x(j)^2)/p^2;
%     else
%         y(j) = m*((1-2*p)+2*p*x(j)-x(j)^2)/(1-p)^2;
%     end
% end 
% % SOLUTION: The lift and moment coefficients are
% % CL = 0.78292
% % CM about the quarter chord = -0.14681
%
% Problem 7: Parabolic (ot circular-arc) camber
fc = 0.025; alpha = 0;
for j=1:N+1; y(j)=4*fc*x(j)*(1-x(j))- x(j)*sin(alpha*pi/180);end 
% SOLUTION: The lift and moment coefficients are
% CL = 0.312
% CM about the quarter chord = -0.078
%
% % Problem of Section 4.5, page 182: Geometry of a flat plate 
% % with a flap 
% xhinge = 0.5; mxdeflect = 0.05; alpha = -1.45;
% for j=1:N+1 
%     if x(j) <= xhinge
%         y(j) = 0 - x(j)*sin(alpha*pi/180);
%     else
%         y(j)= -mxdeflect*(x(j)-xhinge) - x(j)*sin(alpha*pi/180);
%     end
% end 
% % SOLUTION: The lift and moment coefficients are 
% % CL = 0.097584 
% % CM about the quarter chord = -0.025124
% % % Check on CL [Result checks with Equation (4.52) on pg. 183.] 
% %  F = 1 - xhinge;
% %  eta = atan(F*mxdeflect/F);
% %  cosphi = 1 - 2 * (1-F);
% %  phi = acos(cosphi);
% %  disp('Check on CL')
% %  CL = 2*pi*alpha*pi/180 + 2*pi*eta*(1-phi/pi) + 2*eta*sin(phi) 
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
figure(1)
plot(xvort,yvort,'xr'), hold on
plot(xc,yc,'o')
hold off
figure(2)
delP = -vortex_strength_vector./dsP';
plot(xvort,delP,'r')
title('Pressure jump across surface of thin plate')
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