function scheibe26
clf, hold on
rw = 1; alfa = pi/9; m = 0.25; z = 8;
rw = 34; alfa = pi/9; m = 4; z = 17; c = 0.25*m;
rG = rw*cos(alfa); rF = rw - m - c; rK = rw + m;
% -- Bildjustierung -----------------------
qq = 1.2; % Faktor
plot(-qq*rw,-qq*rw,'k.'), hold on
plot(qq*rw,qq*rw,'k.'), hold on
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
xi1 = sqrt((rK^2 - rG^2)/rG^2);
XI = linspace(0,xi1,NN);
XXL = [rF,rG,rG*cos(XI) + rG*XI.*sin(XI);
       0 ,0, rG*sin(XI) - rG*XI.*cos(XI)];
XXR = [rF,rG,rG*cos(XI) + rG*XI.*sin(XI);
       0 ,0, -rG*sin(XI) + rG*XI.*cos(XI)];
beta = m*pi/(4*rw) + tan(alfa) - alfa;
c = cos(beta); s = sin(beta);
D = [c, -s; s, c]; DT = D';
XXL = DT*XXL;
XXR = D*XXR; 
XXR = fliplr(XXR);
       
%beta = 0.5;
%beta = PP/2;
%c = cos(beta); s = sin(beta);
%D = [c, -s; s, c];
%XXR = D*XXR; XXR = fliplr(XXR);
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
RR = rw/20;
circle(0,0,RR,'w')
RRA = rw; 
save datena XXA RRA
