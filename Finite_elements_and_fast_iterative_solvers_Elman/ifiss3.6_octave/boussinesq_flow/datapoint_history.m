function [ttch,uxh,uyh,dph,Th]= ...
  datapoint_history(ttc,uxc,uyc,pc,Tc,eh1,eh2,phih1,psih1,phih2,...
                      mv2,mp1,mt2,mt1,qmethod,ttch,uxh,uyh,dph,Th);
%DATAPOINT_HISTORY interpolation for flow data at two points 
%   IFISS function: DJS; 3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
ux=0; uy=0;
for k=1:9   %  Q2 velocity interpolation
   ux=ux+psih1(k)*uxc(mv2(eh1,k));
   uy=uy+psih1(k)*uyc(mv2(eh1,k));
end
p1=0; p2=0;
for k=1:4   %  Q1 pressure interpolation
   p1=p1+phih1(k)*pc(mp1(eh1,k));
   p2=p2+phih2(k)*pc(mp1(eh2,k));
end 
dp=p1-p2;
switch(qmethod)
    case(1)    %  Q2 temperature interpolation
       T=0;
       for k=1:9
          T=T+psih1(k)*Tc(mt2(eh1,k));
       end
    case(2)    %  Q1 temperature interpolation
       T=0; 
       for k=1:4
          T=T+phih1(k)*Tc(mt1(eh1,k));
       end
end
ttch=[ttch;ttc];uxh=[uxh;ux];uyh=[uyh;uy];dph=[dph;dp];Th=[Th;T];
return
