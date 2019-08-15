function bld061201
clc, clear,
clf, hold on
rw = 34; alfa = pi/9; m = 4; z = 17; c = 0.25*m;
rG = rw*cos(alfa);
rF = rw - m - c;
rK = rw + m;

RW = 162; alfa = pi/9; m = 4; Z = 81;
RG = RW*cos(alfa);
RF = RW - m;
RK = RW + m;
RF_RG = [RF,RG]
TT = linspace(0,2*pi,80);
% -- Waelzkreise -----------------------
X1 = - RW + RW*cos(TT); Y1 = RW*sin(TT);
X2 = rw + rw*cos(TT); Y2 = rw*sin(TT);
plot(X1,Y1,'k','linewidth',2), hold on
plot(X2,Y2,'k','linewidth',2), hold on
% -- Kopfkreise --------------
X1 = - RW + RK*cos(TT); Y1 = RK*sin(TT);
X2 = rw + rK*cos(TT); Y2 = rK*sin(TT);
plot(X1,Y1,'g','linewidth',2), hold on
plot(X2,Y2,'g','linewidth',2), hold on
% -- Fusskreise --------------
X1 = - RW + RF*cos(TT); Y1 = RF*sin(TT);
X2 = rw + rF*cos(TT); Y2 = rF*sin(TT);
plot(X1,Y1,'r','linewidth',2), hold on
plot(X2,Y2,'r','linewidth',2), hold on
% -- Grundkreise --------------
X1 = - RW + RG*cos(TT); Y1 = RG*sin(TT);
X2 = rw + rG*cos(TT); Y2 = rG*sin(TT);
plot(X1,Y1,'b','linewidth',2), hold on
plot(X2,Y2,'b','linewidth',2), hold on
% -- Teilung --------------
PP = 2*pi/Z;
for I = 1:Z
    X1 = - RW + cos((I-1)*PP)*[RF, RK];
    Y1 = sin((I-1)*PP)*[RF, RK];
    plot(X1,Y1,'k'), hold on
end
PP = 2*pi/z;
for I = 1:z
    X1 = rw + cos((pi/z) + (I-1)*PP)*[rF, rK];
    Y1 = sin((pi/z) + (I-1)*PP)*[rF, rK];
    plot(X1,Y1,'k'), hold on
end

grid on
RR = 0.05;
circle(0,0,RR,'w')
circle(rw,0,RR,'w')
circle(-RW,0,RR,'w')
axis equal tight
