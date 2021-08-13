function tskew=skewness_check(uxc,uyc,Tc,eh1,eh2,psih1,psih2,mv2,mt2);
%SKEWNESS_CHECK computes point skewness measure for Boussinesq
%   tskew = skewness_check(uxc,uyc,Tc,eh1,eh2,psih1,psih2,mv2,mt2);
%
%   output
%          tskew     temperature skewness measure
%
%  Computes the Q2 interpolation for the point data history
%  at the two points (s1,t1) and (s2,t2)
%   IFISS function: DJS; 29 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
ux1=0; uy1=0; ux2=0; uy2=0;
for i=1:9   %  Q2 velocity interpolation
   ux1=ux1+psih1(i)*uxc(mv2(eh1,i));
   uy1=uy1+psih1(i)*uyc(mv2(eh1,i));
   ux2=ux2+psih2(i)*uxc(mv2(eh2,i));
   uy2=uy2+psih2(i)*uyc(mv2(eh2,i));
end
uxskew = ux1+ux2;
uyskew = uy1+uy2;
T1=0; T2=0;
for i=1:9
    T1=T1+psih1(i)*Tc(mt2(eh1,i));
    T2=T2+psih2(i)*Tc(mt2(eh2,i));
end
tskew = T1+T2;
return
