% Explore a set of directions, display Lagrangians

tfg=B_2D_Haar; %function call

th=5; %threshold
NA=60; %number of angles to test

%the square
rbeg=1; Nr=size(tfg,1);
cbeg=5*Nr/8;
cw=(Nr/8)-1; 
sq=tfg(rbeg:rbeg+cw,cbeg:cbeg+cw);

SLG=B_Bestdir(sq,th,NA); %function call

[minL,iL]=min(SLG);

%display
figure(1)
xa=(1:NA).*(180/NA);
plot(xa(:),SLG(:),'k');
title('Lagrangian vs. angle');
xlabel('angle in degrees');

%print best angle 
angle=(iL*180)/NA 
