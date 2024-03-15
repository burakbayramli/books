%
% Chapter 7 problems  
% Daniel T. Valentine ................... 2009/2016.
clear;clc
% % Problem 1: An elliptic wing problem (see Section 7.5.3).
% L = 73600;% Newtons.
% b = 15.23; % meters.
% V = 90; % m/s.
% s = b/2;
% rho = 1.2256*.9762; % kg/m/m/m at altitude = 250 m.
% Gamma0 = L/(rho*V*pi*s/2); % Equation (5.38) pg. 240.
% Dv = (pi/8)*rho*Gamma0^2/1000; % Equation (5.42) pg. 241.
% G0 = round(Gamma0);
% disp([' (a) Induced drag = ' num2str(Dv) ' kN'] )
% disp([' (b) Mid-span circulation = ' num2str(G0) ' m*m/s'] )
% %
% Problem 2: Glide path problem: The balance equations for 
% gliding without being under power are CL * cos(glide_angle) = CW,
% and CL * sin(glide_angle) = CD. CD = 0.02 + 0.06 CL*CL for AR = 6. 
% How does glide-path angle change with AR? Compare AR = 6 with 
% AR = 10. Assume the wing behaves like an ellitic wing. Thus, for 
% AR = 6, CDI = CL*CL/(pi*AR) = 0.0531*CL^2. For AR = 10 the induced 
% drag is CDI = 0.0318*CL^2. 
% sin(th1) = 0.02/CL + .007*CL + 0.0531*CL;
% sin(th2) = 0.02/CL + .007*CL + 0.0318*CL;
% dsinthe = 0.0213*CL;% sin(th1)-sin(th2);
% For constant CL, th1 > th2 for AR1 < AR2. Since th2 < th1, the higher 
% the AR the smaller is the glide-path angle indicating that the plane 
% with higher AR will glide a longer distance. 
%
% % Problem 3: The ellitpic loading is the minimum induced drag loading 
% % because the the induced velocity is uniform and, thus, delta = 0; 
% % See the discussion of Equation (5.50) on the top of page 248. 
% delta = 0; % Elliptic loading
% AR = 7.63;
% CL = 0:.1:2;
% CDI = (CL.^2)/pi/AR*(1+delta);
% plot(CL,CDI)
% xlabel('CL'),ylabel('CDI')
% title('Induced drag versus lift for elliptic loading')
%
% % % Problem 4: 
% syms x 
% syms L s rho V Gamma0
% fx = 1 - x*x; % Given circulation distribution.
% L  = rho*V*Gamma0*s*int(fx,'x',-1,1)
% fe = sqrt(fx);% Elliptic circulation distribution.
% Le = rho*V*Gamma0*s*int(fe,'x',-1,1)
% % ans = 4/3 as compared with pi/2 for elliptic loading. 
% % For the given loading
% Gamma0 = L/( (4/3)*s*rho*V);
% wm = Gamma0/pi/s
% Gamma0el = L/(rho*V*pi*s/2);
% wmel = Gamma0el/4/s
% % syms L s rho V 
% %wmwe = ( L/( (4/3)*s*rho*V * pi* s) )/ ( L/(rho*V*pi*s*4*s/2) )
%  % wmwe = 3/2
%  % Thus the induced velocity at midspan for the given load distribution 
%  % is 1.5 times the same for the elliptic load distribution.
% %
% Problem 9: Consider a symmetric rectangular wing with AR=5.
% AR = 2s/c for rectangular wing. Let us examine N Fourier 
% terms.
a_infinity = 2*pi; AR = 5; 
alpha = 10*pi/180;
mu = .888*a_infinity/4/AR;
% Matrix of coefficients to compute An's
 N = 11;
 theta = pi/N:pi/N:pi;
for n=1:length(theta)
    for m=1:length(theta)
        AA(n,m) = sin(m*theta(n))*(1+m*mu/sin(theta(n)));
        if abs(AA(n,m)) < 10^-6
            AA(n,m) = 0;
        end
    end
end
for m=1:length(theta)
    b(m) = mu*alpha;
end
AI = inv(AA);
A = AI*b';
CL = A(1)*pi*AR
% Note: if change CL with CL =.691; then CDi is as given.
delta = 3*A(3)^2/A(1)^2 + 5*A(5)^2/A(1)^2 + 7*A(7)^2/A(1)^2 ...
    + 9*A(9)^2/A(1)^2; 
CDI = CL^2/(pi*AR) * (1 + delta)
%
