% Example of wrapping

%circle filled with points
alpha=0:0.1:360; 
alpha=(alpha*2*pi)/360; %in rads
Pcx=cos(alpha)';
Pcy=sin(alpha)';

R=rand(10000,1); %set of radii
alpha=2*pi*rand(10000,1);

Px=R.*cos(alpha); Px=[Px;Pcx];
Py=R.*sin(alpha); Py=[Py;Pcy];

%stretch
Px=0.3*Px;
%rotate
Px=Px+ (0.6*Py);

ra=10001:length(Px); %range

figure(1)
plot(Px,Py,'g.'); hold on;
plot(Px(ra),Py(ra),'k.');
line([-1.2 1.2],[0 0]);
line([0 0],[-1.2 1.2]);
axis([-1.2 1.2 -1.2 1.2]);
title('original curvelet');

%wrapping
WPx=Px;
for j=1:length(Px),
   if Px(j)>0.35, WPx(j)=Px(j)-0.7; end;
   if Px(j)<-0.35, WPx(j)=Px(j)+0.7; end;
end;   

figure(2)
plot(WPx,Py,'g.'); hold on;
plot(WPx(ra),Py(ra),'k.');
line([-1.2 1.2],[0 0]);
line([0 0],[-1.2 1.2]);
%rectangle
axis([-1.2 1.2 -1.2 1.2]);
rectangle('Position',[-0.4 -1.1 0.8 2.2]);
title('wrapped curvelet');



