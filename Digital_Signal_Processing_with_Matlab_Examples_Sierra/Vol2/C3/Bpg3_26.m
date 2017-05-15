% HSV cone

%make an HSV image
Nh=1000; Ns=500; Nv=500;
H=repmat(linspace(0,1,Nh),Nh,1);
S=repmat([linspace(0,1,Ns) linspace(1,0,Ns)].',1,2*Ns);
V=repmat([ones(1,Nv) linspace(1,0,Nv)].',1,2*Nv);

Ihsv=cat(3,H,S,V);

%convert to RGB
Irgb=hsv2rgb(Ihsv);

%make the cone
phi=linspace(0,2*pi,1000);
zz=zeros(1,1000);
X=[zz; cos(phi); zz];
Y=[zz; sin(phi); zz];
Z=[1.*ones(2,1000); zz];

figure(1)
surf(X,Y,Z,Irgb,'FaceColor','texturemap','EdgeColor','none');
axis([ -1 1 -1 1 0 1 ]);
title('HSV cone');
zlabel('value');



