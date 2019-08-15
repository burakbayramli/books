function bld061203
clc, clear,
clf, hold on
rw = 1; RW = 2; alfa = pi/9; m = 1/4; Z = 16; z = 8;
RG = RW*cos(alfa); rG = rw*cos(alfa);
RF = RW - m; rF = rw - m;
RK = RW + m; rK = rw + m;
% -- Bildjustierung -----------------------
rr = 0.02;
circle(-1.5,-1.5,rr,'w')
circle(1.5,1.5,rr,'w')
axis equal tight, axis manual, grid on

TT = linspace(0,2*pi,80);
% -- Waelzkreise -----------------------
X2 = rw*cos(TT); Y2 = rw*sin(TT);
plot(X2,Y2,'k','linewidth',2), hold on
% -- Kopfkreise --------------
X2 = rK*cos(TT); Y2 = rK*sin(TT);
plot(X2,Y2,'g','linewidth',2), hold on
% -- Fusskreise --------------
X2 = rF*cos(TT); Y2 = rF*sin(TT);
plot(X2,Y2,'r','linewidth',2), hold on
% -- Grundkreise --------------
X2 = rG*cos(TT); Y2 = rG*sin(TT);
plot(X2,Y2,'b','linewidth',2), hold on
% -- Teilung --------------
PP = 2*pi/z;
% -- Evolvente --------------
NN = 10;
xi1 = tan(acos(rG/rK));
XI = linspace(0,xi1,NN);
RR = rG*sqrt(1 + XI.*XI);
PSI = XI - atan(XI);
XXL = [rF,rG,RR.*cos(PSI); 0, 0,   RR.*sin(PSI)];
XXR = [rF,rG,RR.*cos(PSI); 0, 0, - RR.*sin(PSI)];
beta = 0.5;
beta = PP/2;
c = cos(beta); s = sin(beta);
D = [c, -s; s, c];
XXR = D*XXR; XXR = fliplr(XXR);
XX = [XXL,XXR];
%c1 = cos(PP);
%s1 = sin(PP);
%D = [c1, -s1; s1, c1];
%XX = [XX,D*XXL(:,1)];
XXA = XX;
for I = 2:z
    c1 = cos((I-1)*PP);
    s1 = sin((I-1)*PP);
    D = [c1, -s1; s1, c1];
    XX1 = D*XX;
    XXA = [XXA,XX1];
    %plot(XX1(1,:),XX1(2,:),'b','linewidth',2), hold on
end
XXA = [XXA,XXA(:,1)];
fill(XXA(1,:),XXA(2,:),'y'),hold on
plot(XXA(1,:),XXA(2,:),'b','linewidth',2), hold on

RR = 0.05;
circle(0,0,RR,'w')
