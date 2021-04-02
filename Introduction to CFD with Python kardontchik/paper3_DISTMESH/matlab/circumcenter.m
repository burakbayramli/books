function [pc,r]=circumcenter(p,t)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

nt=size(t,1);
pc=zeros(nt,2);
r=zeros(nt,1);

for it=1:nt
  ct=t(it,:);
  dp1=p(ct(2),:)-p(ct(1),:);
  dp2=p(ct(3),:)-p(ct(1),:);
  
  mid1=(p(ct(2),:)+p(ct(1),:))/2;
  mid2=(p(ct(3),:)+p(ct(1),:))/2;
  
  s=[-dp1(2),dp2(2);dp1(1),-dp2(1)]\[-mid1+mid2]';
  
  cpc=mid1+s(1)*[-dp1(2),dp1(1)];
  cr=norm(p(ct(1),:)-cpc);
  
  pc(it,:)=cpc;
  r(it,1)=cr;
end
