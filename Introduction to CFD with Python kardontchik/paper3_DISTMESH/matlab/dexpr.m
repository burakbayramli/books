function d=dexpr(p,fin,nit,alpha)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

if nargin<3, nit=20; end
if nargin<4, alpha=0.1; end

fx=inline(vectorize(maple(['diff(',fin,',x)'])),'x','y');
fy=inline(vectorize(maple(['diff(',fin,',y)'])),'x','y');
fxx=inline(vectorize(maple(['diff(',fin,',x$2)'])),'x','y');
fyy=inline(vectorize(maple(['diff(',fin,',y$2)'])),'x','y');
fxy=inline(vectorize(maple(['diff(diff(',fin,',x),y)'])),'x','y');
f=inline(vectorize(fin),'x','y');

x0=p(:,1);
y0=p(:,2);
x=x0;
y=y0;
for it=1:nit
  cf=feval(f,x,y);
  cfx=feval(fx,x,y);
  cfy=feval(fy,x,y);
  cfxx=feval(fxx,x,y);
  cfxy=feval(fxy,x,y);
  cfyy=feval(fyy,x,y);

  F1=cf;
  F2=(x-x0).*cfy-(y-y0).*cfx;
  J11=cfx;
  J12=cfy;
  J21=cfy+(x-x0).*cfxy-(y-y0).*cfxx;
  J22=-cfx-(y-y0).*cfxy+(x-x0).*cfyy;
  
  detJ=J11.*J22-J12.*J21;
  detJ(detJ==0)=inf;
  
  x=x-alpha*(J22.*F1-J21.*F2)./detJ;
  y=y-alpha*(-J12.*F1+J11.*F2)./detJ;
end

d=sqrt((x-x0).^2+(y-y0).^2).*sign(feval(f,x0,y0));
