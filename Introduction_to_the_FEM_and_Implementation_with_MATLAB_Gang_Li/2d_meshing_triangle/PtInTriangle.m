function [re,a,b]=PtInTriangle(p1, p2, p3, p)
global TOL;
v0=p1;
v1=p2-p1;
v2=p3-p1;
detpv1=p(1)*v1(2) - p(2)*v1(1);
detpv2=p(1)*v2(2) - p(2)*v2(1);
detv0v1=v0(1)*v1(2) - v0(2)*v1(1);
detv0v2=v0(1)*v2(2) - v0(2)*v2(1);
detv1v2=v1(1)*v2(2) - v1(2)*v2(1);
a=(detpv2 - detv0v2)/detv1v2;    % v=v_0 + a*v_1 + b*v_2
b=-(detpv1 - detv0v1)/detv1v2;   % cofficients a and b

% if-block: determine return value based on different situations
if abs(a)<TOL && b<=1 && b>=0
  re=0;
elseif abs(b)<TOL && a<=1 && a>=0
  re=0;
elseif abs(a+b-1)<TOL && a<=1 && a>=0 && b<=1 && b>=0
  re=0;                                % on triangle
elseif a>0 && b>0 && a+b<1
  re=1;                                % in triangle
else
  re=-1;                               % outside of triangle
end