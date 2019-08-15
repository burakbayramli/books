function fig0220
% Figure 2.20
clf, clear
% Data of example --------------
m = 20; n = 2;
Maxwert   = 10;  %in BILD025: Maxwert = 10;
TT        = linspace(0,1,m+1);
X0        = [TT;ones(1,m+1)];
Parmeter3 = [];
Flag      = 1;
options   = ...
   odeset('reltol',1.0E-3,'abstol',1.0E-6,'outputfcn',@mybound);
% --------------------------------
c = 0.05; d = 0.3;
% -- X-Axis -----------------
X1 = [-0.2,1.2]; Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Axis ------------------
c = 0.7; d = 0.015;
X1 = [0,0]; Y1 = [-1,13];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Straight connection ----------
plot(TT,TT,'k','linewidth',2), hold on
% -- Upper boundary ----------
TT1 = TT+Maxwert;
plot(TT,TT1,'k--','linewidth',2), hold on
% ---- 1. Solution curve  -----------------------
Flag = 1;
ANF  = [0;1];
[TA,YA] = ode23(@bsp01,[0,1],ANF,options,Flag,Parmeter3);
TA = TA'; YA = YA';
TL = length(TA);
XA      = zeros(n,TL);
for K = 1:n
   XA(K,:) = interp1(TT,X0(K,:),TA);
end
DIFF     = YA - XA;
DIFFNORM = zeros(1,TL);
for K = 1:TL
   DIFFNORM(K) = norm(DIFF(:,K),inf);
end
AUX = max(find(DIFFNORM <= Maxwert));
MZP1 = TA(AUX);
ANF = zeros(n,1);
for K = 1:n
   ANF(K) = interp1(TT,X0(K,:),MZP1);
end
plot(ANF(1),ANF(1),'k*','markersize',8), hold on
% -- 2. Solution curve ----------------------
[TB,YB] = ode23(@bsp01,[MZP1, 1],ANF,options,Flag,Parmeter3);
TB = TB';
YB = YB';
TL = length(TB);
XB      = zeros(n,TL);
for K = 1:n
   XB(K,:) = interp1(TT,X0(K,:),TB);
end
DIFF     = YB - XB;
DIFFNORM = zeros(1,TL);
for K = 1:TL
   DIFFNORM(K) = norm(DIFF(:,K),inf);
end
AUX = max(find(DIFFNORM <= Maxwert));
MZP2 = TB(AUX);
ANF = zeros(n,1);
for K = 1:n
   ANF(K) = interp1(TT,X0(K,:),MZP2);
end
plot(ANF(1),ANF(1),'k*','markersize',8), hold on
% -- Output -------------------------
plot(TA,YA(2,:),'k','linewidth',2), hold on
plot(TB,YB(2,:),'k','linewidth',2), hold on
X3 = [MZP1,MZP1];
Y3 = [-1,13];
plot(X3,Y3,'k:'), hold on
X3 = [MZP2,MZP2];
Y3 = [-1,13];
plot(X3,Y3,'k:'), hold on
%grid on
text(0.6,12,'bound','fontsize',22)
text(0.58,6,'x_2(t)','fontsize',22)
text(1.1,-1,'t','fontsize',22)
text(0.3,-1,'shooting points','fontsize',22)

%title('Beispiel 1','fontsize',18)
