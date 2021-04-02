function meshdemond
%MESHDEMOND distmeshnd examples.

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

rand('state',1); % Always the same results
set(gcf,'rend','opengl');

disp('(9) 3-D Unit ball')
fd=inline('sqrt(sum(p.^2,2))-1','p');
[p,t]=distmeshnd(fd,@huniform,0.2,[-1,-1,-1;1,1,1],[]);
post(p,t)

disp('(10) Cylinder with hole')
[p,t]=distmeshnd(@fd10,@fh10,0.1,[-1,-1,-1;1,1,1],[]);
post(p,t)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function post(p,t)

disp(sprintf('   (press any key)'))
disp(' ')
pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function d=fd10(p)

r=sqrt(p(:,1).^2+p(:,2).^2);
z=p(:,3);

d1=r-1;
d2=z-1;
d3=-z-1;
d4=sqrt(d1.^2+d2.^2);
d5=sqrt(d1.^2+d3.^2);
d=dintersect(dintersect(d1,d2),d3);
ix=d1>0 & d2>0;
d(ix)=d4(ix);
ix=d1>0 & d3>0;
d(ix)=d5(ix);

d=ddiff(d,dsphere(p,0,0,0,0.5));

function h=fh10(p)

h1=4*sqrt(sum(p.^2,2))-1;
h=min(h1,2);
