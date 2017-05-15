% B set for a given direction

tfg=B_2D_Haar; %function call

th=5; %threshold

%the square
rbeg=1; Nr=size(tfg,1);
cbeg=5*Nr/8;
cw=(Nr/8)-1; 
sq=tfg(rbeg:rbeg+cw,cbeg:cbeg+cw);

%angle
alpha=(130*pi)/180; %rads

wty=B_Bset(sq,alpha); %function call

%display
figure(1)
plot(wty,'k');
axis([0 length(wty) 1.2*min(wty) 1.2*max(wty)]);
title('Example of B set');

%evaluation
Rs=wty.*(abs(wty)<th); %residual
Sa=sum(abs(wty(:)>th)); %value above threshold
Lg=sum(Rs(:).^2)+(Sa*(th^2)); %Lagrangian
