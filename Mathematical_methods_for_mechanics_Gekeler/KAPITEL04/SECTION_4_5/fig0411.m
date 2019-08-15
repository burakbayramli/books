function fig0411
% Figure 4.11

disp(' Call first DEMO1-7 ! ')
clf
load daten07 X Parmeter
n    = Parmeter(1);
alfa = Parmeter(2);
A           = 3/n;
FAKTOR      = ones(1,n+1);
FAKTOR(1)   = 0.5;
FAKTOR(n+1) = 0.5;
FAKTOR      = 3*FAKTOR/n;
% -- Naeherungsloesung --------------------------
X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
U  = X(2*n+3:3*(n+1));
TSPAN   = linspace(0,3,n+1)';
plot(TSPAN,X1,'ko-','Linewidth',2,'markersize',6), hold on
plot(TSPAN,X2,'ko-','Linewidth',2,'markersize',6), hold on
plot(TSPAN,U,'ks-','Linewidth',2,'markersize',6), hold on
% -- EXAKTE LOESUNG -----------
if alfa <= -7
   T1  = linspace(0,3,n+1);
   X1  = [2 - T1.*T1; - 2*T1];
   U1  = - 2*ones(1,n+1);
   plot(T1,X1(1,:),'k',T1,X1(2,:),'k','Linewidth',2), hold on
   plot(T1,U1,'k*-','Linewidth',2,'markersize',6), hold on
   PERF_INDEX  = 2*FAKTOR*U(1:n+1)
   disp('Exakter Wert = -6')
end
if alfa > -7 & alfa <= - 2.5
   sigma = 3 - sqrt(56 + 8*alfa)/4;
   T1 = linspace(0,3,n+1);
   SS = max(find(T1 <= sigma));
   J = [1:SS];
   K = [SS:n+1];
   TJ = T1(J);
   X1 = [2 - T1.*T1; - 2*T1];
   U1 = - 2*ones(n+1,1);
   plot(TJ,X1(1,J),'k',TJ,X1(2,J),'k','Linewidth',2), hold on
   plot(TJ,U1(J),'k*-','Linewidth',2,'markersize',6), hold on
   % -------------------------------------
   TK = T1(K);
   X1 = [2 + T1.*T1 + 2*sigma^2 - 4*sigma*T1;
         2*(T1 - 2*sigma)];
   U1 =  2*ones(n+1,1);
   plot(TK,X1(1,K),TK,X1(2,K),'k','Linewidth',2), hold on
   plot(TK,U1(K),'b','Linewidth',2), hold on
   PERF_INDEX  = 2*FAKTOR*U(1:n+1)
   F = bsp07b(Parmeter);
   EXAKTER_WERT = F
end
if alfa > - 2.5 & alfa <= 0
   sigma = sqrt(4 - 2*alfa)/2;
   T1  = linspace(0,3,n+1);
   SS = max(find(T1 <= sigma));
   RR = max(find(T1 <= 2*sigma));
   J  = [1:SS];
   K  = [SS:RR+1];
   L  = [RR+1:n+1];
   TJ = T1(J);
   TK = T1(K);
   TL = T1(L);
   X1 = [2 - T1.*T1; - 2*T1];
   U1 = - 2*ones(1,n+1);
   plot(TJ,X1(1,J),'k',TJ,X1(2,J),'k','Linewidth',2), hold on
   plot(TJ,U1(J),'k*-','Linewidth',2,'markersize',6), hold on
   % --------------------------------
   X1 = [2 + T1.*T1 + 2*sigma^2 - 4*sigma*T1;
         2*(T1 - 2*sigma)];
   U1 =  2*ones(1,n+1);
   plot(TK,X1(1,K),'k',TK,X1(2,K),'k','Linewidth',2), hold on
   plot(TK,U1(K),'k*-','Linewidth',2,'markersize',6), hold on
   % --------------------------------
   X1 = [alfa*ones(1,n+1);
         zeros(1,n+1)];
   U1 =  zeros(1,n+1);
   plot(TL,X1(1,L),'k',TL,X1(2,L),'k','Linewidth',2), hold on
   plot(TL,U1(L),'k*-','Linewidth',2,'markersize',6), hold on
    PERF_INDEX  = 2*FAKTOR*U(1:n+1)
    F = bsp07b(Parmeter);
    EXAKTER_WERT = F
    text(0.6,2,'x*_1','fontsize',22)
    text(0.6,-0.8,'x*_2','fontsize',22)
    text(1.5,-1.2,'x*_2','fontsize',22)
    text(2,0.5,'x*_2','fontsize',22)

    text(0.5,-2.5,'u*','fontsize',22)
    text(1.8,1.6,'u*','fontsize',22)
    text(2.7,-0.4,'u*','fontsize',22)
    text(1.8,1.6,'u*','fontsize',22)
    text(2.7,-0.4,'u*','fontsize',22)

    text(1.05,-0.1,'u','fontsize',22)
    text(2.2,2.5,'u','fontsize',22)
    X8 = [2.15,2.7];
    Y8 =[0.5,0.25];
    c = 0.1; d = 0.05;
    arrow(X8,Y8,c,d,'k',1)
end
axis ([0 3 -3 3])
grid off
