function scheibe27
clf, hold on
rw = 2; alfa = pi/9; m = 1/4; z = 16;
rG = rw*cos(alfa); rF = rw - m; rK = rw + m;
% -- Bildjustierung -----------------------
rr = 0.02;
circle(-2.5,-2.5,rr,'w')
circle(2.5,2.5,rr,'w')
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
c = cos(PP); s = sin(PP);
D = [c, -s; s, c];

XX = [XXL,XXR];
XXA = XX;
for I = 2:z
    I
    c1 = cos((I-1)*PP);
    s1 = sin((I-1)*PP);
    D = [c1, -s1; s1, c1];
    XX1 = D*XX;
    XXA = [XXA,XX1];
    %plot(XXA(1,:),XXA(2,:),'b','linewidth',2), hold on
    %pause
end
XXA = [XXA,XXA(:,1)];
for I = 1:size(XXA,2)-1
    AA = XXA(1,I:I+1); BB = XXA(2,I:I+1);
    plot(AA,BB,'b'), hold on
end


RR = 0.05;
circle(0,0,RR,'w')
%save datenb XXB
