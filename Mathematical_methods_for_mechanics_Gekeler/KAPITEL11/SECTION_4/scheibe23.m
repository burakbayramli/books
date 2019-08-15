% Scheibe 23, KKK-Ro-Lader System Felix Wankel
clf
M = [0;0]; R = 4; % Mittelpunkt/Radius
NN  = 13;      % Anzahl Intervalle
FAKTOR = 1.2; % Bildgroesse
TT1 =  linspace(0,2*pi,100);
X1  = R*[cos(TT1);sin(TT1)];
plot(X1(1,:),X1(2,:),'r','linewidth',2), hold on
axis equal, grid on, hold on
A = R*[cos(pi/3-DPHI); sin(pi/3- DPHI)];
B = [0;2];;
TT5 = linspace(0,1,MM);
SEGM1A = A*ones(1,MM) + (B - A)*TT5;
SEGM1A = SEGM1A(:,1:MM-1);
C = R*[cos(2*pi/3 + DPHI); sin(2*pi/3 + DPHI)];
SEGM1C = B*ones(1,MM) + (C - B)*TT5;
SEGM1 = [SEGM1A,SEGM1C];
TT6 = linspace(2*pi/3+DPHI,pi-DPHI,MM);
SEGM2 = R*[cos(TT6);sin(TT6)];
SEGM2 = SEGM2(:,2:MM-1);
cs = cos(2*pi/3); ss = sin(2*pi/3);
DD = [cs, - ss; ss, cs];
SEGM3 = DD*SEGM1; SEGM4 = DD*SEGM2;
SEGM5 = DD*SEGM3; SEGM6 = DD*SEGM4;
X4 = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6];
cs = cos(pi/6); ss = sin(pi/6);
DD = [cs, ss; - ss, cs];
%X4 = DD*DD*X4;
plot(X4(1,:),X4(2,:),'b','linewidth',2), hold on
circle(0,0,0.07,'w')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load scheibe19 M R X
M2 = [0;1];
X = X + M2*ones(1,size(X,2));
plot(X(1,:),X(2,:),'b'),hold on
circle(M2(1),M2(2),0.07,'r')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LL = 2; % Abstand der Zentren
RR = sqrt(2*LL*LL)/2;
TT = linspace(-pi/4,-3*pi/4,MM); SEGM4 = [RR*cos(TT);RR*sin(TT)];
M4 = [0;LL+1]; SEGM4 = M4*ones(1,MM) + SEGM4;
%SEGM4 = SEGM4(:,1:MM-1);
X5 = SEGM4;
plot(X5(1,:),X5(2,:),'k','linewidth',2),hold on
circle(0,LL+1,0.07,'w')
FAKTOR = 1.2; % Bildgroesse

LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
flag = 0;
if flag == 1
   pause
   for I = 1:size(X4,2)
      plot(X4(1,I),X4(2,I),'.k'), hold on
      pause
   end
end
X = X4;
save scheibe23 M R X
