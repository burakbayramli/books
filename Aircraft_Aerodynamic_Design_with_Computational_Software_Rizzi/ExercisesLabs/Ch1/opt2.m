close all
clear all
format compact
clc
global DEMO WoSor2 XTab Itno
% Demostrates optimization by finding
% alpha and airspeed V (dynamic pressure q = 1/2 rho-air V^2)
% to minimize thrust for straight and level flight
% The aerodynamics are given by curves CL(alpha) and CD(aLpha)
% wing area S m2, weight W (N), and air density sea level rho0 kg/m3
% so X = (alpha,V)
% q = 1/2 rho V^2
% T = D = q S CD(alpha)
% W = L = q S CL(alpha)
% Prop = (CL,CD)
% FoM  = q S CD
% Constr: q S CL >= W
%====
% Since S and W are given, equivalent to
% min V^2 CD
% Prop = (CL,CD) = cfd(X)
% FoM  = X(2)^2 Prop(2)
% Constr: X(2)^2*Prop(1) >= W/[S(1/2 rho)^2 ]
% lower and upper bounds for X
%     lb(1) =  alphamin <= X(1) <= alphamax = ub(1)
%     lb(2) = qmin < = X(2) <= qmax = ub(2)

% Octave fmincon uses:
%function FoM = objf(x)
%  global DEMO
%  if DEMO
%    prop = cfd(x);
%    FoM  = x(2)^2*prop(2);
%    [x' FoM]
%  else
%    error('only DEMO impl. in objf')
%  end

%==================
%function prop = cfd(x)
%global DEMO
%if DEMO
%% cl,cd tables given here:
%  clcd = [...];
%  prop = interp1(clcd(:,1),clcd(:,2:3),x(1));
%else
%  error('only DEMO impl. in cfd')
%end

%==================
%function [cn cl] = constr(x)
%global WoSor2 DEMO
%if DEMO
%  prop = cfd(x);
%  cn   = -x(2)^2*prop(1) + WoSor2;
%  cl   = [];
%else
%  error('only DEMO impl. in constr')
%end

DEMO = 2

% Piper Cub
S    = 16.6  % m2
MTOW = 550   % kg
b    = 10.74 % m
AR   = b^2/S
g    = 9.81       % m/s2
W    = MTOW*g     % N

alphamin   =  -3*pi/180
alphamax   =  10*pi/180
vmin       =   20
vmax       =  100
thetamin   = 0
thetamax   = 20*pi/180
Tmin       = 0
Slist  = [14 16 18] % m2 Wing area
rhoo2  = 0.6        % kg/m3 sea level
r = S*rhoo2
Tmax = 0.1*W/r
lb = [alphamin;vmin;thetamin;Tmin];
ub = [alphamax;vmax;thetamax;Tmax];
x      = [3*pi/180;40;3*pi/180; Tmax*0.8] ; % initial guess
disp(['alpha : ' num2str(x(1)*180/pi) ' speed : ' num2str(x(2)) '... that will not fly'])
opts   = optimset('algorithm','active-set');
xtab= zeros(length(Slist),size(x,1));
ns = length(Slist);
for k = 1:ns
  S = Slist(k);
  WoSor2 = W/S/rhoo2
  x      = [3*pi/180;70;10*pi/180; Tmax*0.8];
  prop = cfd(x);
  % T*cos(x(1)) - V^2*prop(2) = WoSor2*sin(x(3));
  % T*sin(x(1)) + V^2*prop(1) = WoSor2*cos(x(3)); 
  [cn cl] = constr(x)

  XTab   = zeros(1,size(x,1)+1);
  Itno   = 0;
  x1     = fmincon (@objf, x, [], [], [], [], lb, ub, @constr,opts);
  [cc ccc] = constr(x1)
  xtab(k,:) = x1';
  disp(['but this will: opt alpha: ',num2str(180/pi*x1(1)), ' opt speed: ',num2str(x1(2))])
  plot3(XTab(:,2)*180/pi,XTab(:,3),1:size(XTab,1),'x-k');
  hold on
end
xlabel('alpha');
ylabel('TAS')
zlabel('It.')
set(gca,'fontsize',16)
grid
figure(2)
plot(XTab(:,1),'.-k','linewidth',2)

alphlist = [-4:15]*pi/180;
nl   = length(alphlist);
clcd = zeros(nl,3);
for k=1:nl
  alph = alphlist(k);
  tmp  = cfd([alph;10]);
  clcd(k,:) = [alph tmp];
end
figure(3)
plot(clcd(:,3),clcd(:,2),'.-k','linewidth',2)
hold on
for k = 1:ns
clcd1 = interp1(clcd(:,1),clcd(:,2:3),xtab(k,1));
plot(clcd1(2),clcd1(1),'or','markersize',14)
plot([0 2*clcd1(2)],[0 2*clcd1(1)],'--k','linewidth',2)
end
set(gca,'fontsize',16)
grid

