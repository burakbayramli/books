function bild03
clc, clear
load scheibe26 XXA RRA % kleine Scheibe
load scheibe27 XXB RRB % grosse Scheibe
% Kreisradien fuer kleine Scheibe
alfa = pi/9; mod = 4; c = 0.25;

rW = RRA;
rG = rW*cos(alfa); % Radius Grundkreis
rF = rW - mod - c*mod; % Radius Fusskreis
rK = rW + mod;       % Radius Kopfkreis
z = 17;
% Kreisradien fuer grosse Scheibe
RW = RRB;
RG = RW*cos(alfa); % Radius Grundkreis
RF = RW - mod - c*mod; % Radius Fusskreis
RK = RW + mod;       % Radius Kopfkreis
Z = 81;

% -- Bildjustierung -----------------------
clf, hold on
rr = 0.02;
plot(-20,-20,'w.'), hold on
plot(20,20,'w.'), hold on

axis equal tight, axis manual, grid on
M = [-RRB;0]; m = [RRA;0];

% -- Waelzkreise -----------------------
TT = linspace(0,2*pi,80);
X1 = [rW*cos(TT); rW*sin(TT)];
X1 = X1 + m*ones(1,80);
plot(X1(1,:),X1(2,:),'r','linewidth',2), hold on
X1 = [RW*cos(TT); RW*sin(TT)];
X1 = X1 + M*ones(1,80);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on

% -- Zahnraeder --------------------
XXA = XXA + m*ones(1,size(XXA,2));
XXB = XXB + M*ones(1,size(XXB,2));
plot(XXA(1,:),XXA(2,:),'r','linewidth',2), hold on
plot(XXB(1,:),XXB(2,:),'k','linewidth',2), hold on
%fill(XXB(1,:),XXB(2,:),'y')

plot(XXA(1,347),XXA(2,347),'ro'), hold on
plot(XXB(1,31),XXB(2,31),'ko'), hold on


RR = 0.05;
circle(0,0,RR,'w')



