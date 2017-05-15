% Project square on PP'

tfg=B_2D_Haar; %function call
%the square
rbeg=1; Nr=size(tfg,1);
cbeg=5*Nr/8;
cw=(Nr/8)-1; 
sq=tfg(rbeg:rbeg+cw,cbeg:cbeg+cw);

%projection
alpha=(120*pi)/180; %rads
np=cw+1;
[YY,XX]=meshgrid(1:np,1:np);
p=(-sin(alpha)*XX(:))+ (cos(alpha)*YY(:));
[aux,ix]=sort(p); %ascending order
F=sq(ix); %ordered values

figure(1)
plot(F,'k');
axis([0 length(F) 1.2*min(F) 1.2*max(F)]);
title('Example of 1-D function F');
