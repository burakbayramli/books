% Square root covariance filtering "shootout" on an
% ill conditioned problem from P. Dyer & S. McReynolds,
% "Extension of square-root filtering to include process noise"
% Journal of Optimization Theory and Applications, Vol. 3, pp. 444-458, 1969.
%
% Example uses measurement sensitivity matrix
%     [ 1  1     1    ]
% H = [               ], which has pseudorank = 1 when delta = eps.
%     [ 1  1  1+delta ]
%
%
% "eps" is a Matlab reserved name for machine precision limit
% (largest eps such that 1+eps == 1 in machine precision)
%
clear all; close all;
delta = eps^(2/3);
%
% delta    is such that delta>eps but delta^2<eps, so that
% H        has pseudorank == 2 in machine precision, but
% H*P*H'   has pseudorank == 1 in machine precision and
% H*P*H'+R is effectively singular in machine precision.
%
% The observational update is performed using nine methods:
%
%   1. The "conventional" Kalman filter.
%   2. The Swerling inverse implementation.
%   3. The "Joseph stabilized" implementation.
%   4.   --- as modified by Bierman (not plotted).
%   5.   --- as modified by De Vries (not plotted).
%   6. The Potter algorithm listed in [5].
%   7. The Carlson algorithm listed in [145].
%   8. The Bierman algorithm listed in [7].
%   9. The closed form solution (for this particular problem).
%
% These alternative approaches are each evaluated for delta
% ranging from several order of magnitude larger than eps^(2/3) 
% to several orders of magnitude smaller than eps^(2/3) 
%
  for k=1:19,
  d(k) = delta*10^(10-k);
%
% COMMON PARAMETERS TO ALL METHODS
%
  P  = eye(3);
  D0 = eye(3);
  U0 = eye(3);
  R  = d(k)^2*eye(2);
  H  = [1,1,1;1,1,1+d(k)];
  x0 = [1;1;-1];
  z  = H*x0 + d(k)*[randn;randn];
%
% CLOSED FORM SOLUTION
%
  KCF = (1/(2*d(k)*(d(k)^2+d(k)+4)))*[2*d(k)+1,d(k)-1;2*d(k)+1,d(k)-1;d(k)-2,d(k)^2+d(k)+2];
  xCF = x0 + KCF*(z - H*x0);
  PCF = (1/(8+2*d(k)*(1+d(k))))*[5+2*d(k)*(1+d(k)),-3,-2-d(k);-3,5+2*d(k)*(1+d(k)),-2-d(k);-2-d(k),-2-d(k),4+d(k)^2];
%
% CONVENTIONAL KALMAN FILTER IMPLEMENTATION
%
  K  = P*H'/(H*P*H'+R);
  xK = x0 + K*(z - H*x0);
  PK = P - K*H*P;
%
% SWERLING IMPLEMENTATION
%
  PS = inv(P + (H'/R)*H);
%
%JOSEPH STABILIZED IMPLEMENTATION
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
  [K1,P1] = joseph(z1,R1,H1,P);
  [K2,PJ] = joseph(z2,R2,H2,P1);
%
% JOSEPH-BIERMAN STABILIZED IMPLEMENTATION (p. 211)
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
  [K1,P1] = josephb(z1,R1,H1,P);
  [K2,PJ1]= josephb(z2,R2,H2,P1);
%
% JOSEPH - DE VRIES STABILIZED IMPLEMENTATION (p. 212)
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
  [K1,P1] = josephdv(z1,R1,H1,P);
  [K2,PJ2]= josephdv(z2,R2,H2,P1);
%
% POTTER IMPLEMENTATION [5]
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
C0      = chol(P)';
[x1,C1] = potter(z1,R1,H1,x0,C0);
[x2,C2] = potter(z2,R2,H2,x1,C1);
PP      = C2*C2';
%
% CARLSON IMPLEMENTATION [145]
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
C0      = utchol(P);
[x1,C1] = carlson(z1,R1,H1,x0,C0);
[x2,C2] = carlson(z2,R2,H2,x1,C1);
PC      = C2*C2';
%
% BIERMAN IMPLEMENTATION [7]
%
  z1 = z(1);
  z2 = z(2);
  H1 = H(1,:);
  H2 = H(2,:);
  R1 = R(1,1);
  R2 = R(2,2);
  [x1,U1,D1] = bierman(z1,R1,H1,x0,U0,D0);
  [x2,U2,D2] = bierman(z2,R2,H2,x1,U1,D1);
  PB = U2*D2*U2';
%
% Compute and plot relative differences
%
  rssCF = 0; % for closed form solution
  rssS  = 0; % for Swerling solution
  rssK  = 0; % for Kalman solution
  rssJ  = 0; % for Joseph solution
  rssJ1 = 0; %   -- a modified by Bierman
  rssJ2 = 0; %   -- a modified by De Vreis
  rssP  = 0; % for Potter solution
  rssC  = 0; % for Carlson solution
  rssB  = 0; % for Bierman solution
    for i=1:3,
      for j=1:3,
      rssCF = rssC + PCF(i,j)^2;
      if (isnan(PS(i,j))) % set any "Not a Number" to zero
        PS(i,j)=0;
      end;
      if (isinf(PS(i,j))) % set any "Inf" to 10^5
        PS(i,j)=1e5;
      end;
      if (isnan(PK(i,j)))
        PK(i,j)=0;
      end;
      if (isnan(PJ(i,j))) 
        PJ(i,j)=0;
      end;
      if (isnan(PJ1(i,j)))
        PJ1(i,j)=0;
      end;
      if (isnan(PJ2(i,j)))
        PJ2(i,j)=0;
      end;
      if (isnan(PB(i,j))) 
        PB(i,j)=0;
      end;
      rssS  = rssS + (PS(i,j)  - PCF(i,j))^2;
      rssK  = rssK + (PK(i,j)  - PCF(i,j))^2;
      rssJ  = rssJ + (PJ(i,j)  - PCF(i,j))^2;
      rssJ1 = rssJ1 + (PJ1(i,j) - PCF(i,j))^2;
      rssJ2 = rssJ2 + (PJ2(i,j) - PCF(i,j))^2;
      rssP  = rssP + (PP(i,j)  - PCF(i,j))^2;
      rssC  = rssC + (PC(i,j)  - PCF(i,j))^2;
      rssB  = rssB + (PB(i,j)  - PCF(i,j))^2;
      end;
    end;
%
% Compute relative differences and limit rel. error to 10^4
%
  rdS(k)  = sqrt(rssS/rssCF);
    if (rdS(k) > 1e4) rdS(k)=1e4;end;
  rdK(k)  = sqrt(rssK/rssCF);
  rdJ(k)  = sqrt(rssJ/rssCF);
    if (rdJ(k) > 1e4) rdJ(k)=1e4;end;
  rdJ1(k) = sqrt(rssJ1/rssCF);
  rdJ2(k) = sqrt(rssJ2/rssCF);
  rdP(k)  = sqrt(rssP/rssCF);
  rdC(k)  = sqrt(rssC/rssCF);
  rdB(k)  = sqrt(rssB/rssCF);
  end;
%
% Plot relative differences versus delta
%
top    = 10^ceil(log10(max([max(rdS),max(rdK),max(rdJ),max(rdP),max(rdC),max(rdB)])));
bottom = 10^floor(log10(min([min(rdS),min(rdK),min(rdJ),min(rdP),min(rdC),min(rdB)])));
left   = 10^floor(log10(min(d)));
right  = 10^ceil(log10(max(d)));
loglog([right/50000,right/2000],[top/72900000,top/72900000],'r-',[right/50000,right/2000],[top/2430000,top/2430000],'g-',[right/50000,right/2000],[top/81000,top/81000],'b-',[right/50000,right/2000],[top/2700,top/2700],'y-',[right/50000,right/2000],[top/90,top/90],'k-',[right/50000,right/2000],[top/3,top/3],'k:',d,rdJ,'y-',d,rdP,'b-',d,rdC,'g-',d,rdB,'r-',d,rdK,'k-',d,rdS,'k:');
ylabel('RMS Relative Error in P');
xlabel('delta');
title('Grewal, Weill, & Andrews, Exam. 8.1 with Various Solution Methods');
text(right/1000,top/3,'Swerling');
text(right/1000,top/90,'Kalman');
text(right/1000,top/2700,'Joseph');
text(right/1000,top/81000,'Potter');
text(right/1000,top/2430000,'Carlson');
text(right/1000,top/72900000,'Bierman');
text(eps,bottom,'!');
text(eps/3,10*bottom,['log_2(eps) = ',num2str(log2(eps))]);
text(sqrt(eps),bottom,'!');
text(sqrt(eps)/3,10*bottom,'sqrt(eps)');
%


