%Ellipsoids
%Kalman filter example, in noisy conditions
%state space system model (2 tank system):
A1=1; A2=1; R1=0.5; R2=0.4;
cA=[-1/(R1*A1) 1/(R1*A1); 1/(R1*A2) -(1/A2)*((1/R1)+(1/R2))];
cB=[1/A1; 0]; cC=[1 0; 0 1]; cD=0;
Ts=0.1; %sampling period
csys=ss(cA,cB,cC,cD); %setting the continuous time model
dsys=c2d(csys,Ts,'zoh'); %getting the discrete-time model
[A,B,C,D]=ssdata(dsys); %retrieves discrete-time model matrices

%simulation horizon
Nf=10;

%process noise
Sw=[12e-4 0; 0 6e-4]; %cov
sn=zeros(2,Nf); 
sn(1,:)=sqrt(Sw(1,1))*randn(1,Nf);
sn(2,:)=sqrt(Sw(2,2))*randn(1,Nf);
%observation noise
Sv=[6e-4 0; 0 15e-4]; %cov.
on=zeros(2,Nf);
on(1,:)=sqrt(Sv(1,1))*randn(1,Nf);
on(2,:)=sqrt(Sv(2,2))*randn(1,Nf); 

% system simulation preparation
%space for recording x1(n), x2(n)
x1=zeros(1,Nf-1); x2=zeros(1,Nf-1);  
x=[1;0]; % state vector with initial tank levels
u=0.4; %constant input

% Kalman filter simulation preparation
%space for matrices
K=zeros(2,2,Nf); M=zeros(2,2,Nf); P=zeros(2,2,Nf);
aM=zeros(2,2,Nf); cY=zeros(2,2,Nf); 
rxa=zeros(2,Nf); rya=zeros(2,Nf); rym=zeros(2,Nf);
%space for recording er(n), xe1(n), xe2(n)
er=zeros(2,Nf-1); xe1=zeros(1,Nf-1); xe2=zeros(1,Nf-1); 
xe=[0.5; 0.2]; % filter state vector with initial values
   
%behaviour of the system and the Kalman filter after initial state
% with constant input u
for nn=1:Nf-1,
   x1(nn)=x(1); x2(nn)=x(2); %recording the system state
   xe1(nn)=xe(1); xe2(nn)=xe(2); %recording the observer state
   er(:,nn)=x-xe; %recording the error 
   %
   %system simulation
   xn=(A*x)+(B*u)+sn(nn); %next system state
   x=xn; %system state actualization
   ym=(C*x)+on(:,nn); %output measurement
   rym(:,nn)=ym;
   %
   %Prediction
    xa=(A*xe)+(B*u); %a priori state
    M(:,:,nn+1)=(A*P(:,:,nn)*A')+ Sw;
    aM(:,:,nn)=(A*P(:,:,nn)*A');
    rxa(:,nn+1)=xa;   
   %Update 
    K(:,:,nn+1)=(M(:,:,nn+1)*C')*inv((C*M(:,:,nn+1)*C')+Sv);
    cY(:,:,nn+1)=((C*M(:,:,nn+1)*C')+Sv);
    P(:,:,nn+1)=M(:,:,nn+1)-(K(:,:,nn+1)*C*M(:,:,nn+1));
    xe=xa+(K(:,:,nn+1)*(ym-(C*xa))); %estimated (a posteriori) state 
    rya(:,nn+1)=C*xa;
end;  


%---------------------------------------------------------------------

ns1=3; ns2=4; %state number

%Distribution ellipsoids
px1=-0.1:0.02:0.9;
px2=-0.1:0.02:0.4;
pN1=length(px1);
pN2=length(px2);

%the Sw PDF
mu1=0; mu2=0;
C=Sw; D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

Swpdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2));        
     Swpdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%the aP PDF (P(n))
mu1=xe1(ns1); mu2=xe2(ns1);
C=P(:,:,ns1); D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

aPpdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2))...
         -(((px1(ni)-mu1).*(px2(nj)-mu2)/C(1,2)*C(2,1)));
     aPpdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%the M PDF
mu1=rxa(1,ns2); mu2=rxa(2,ns2);
C=M(:,:,ns2); D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

Mpdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2))...
         -(((px1(ni)-mu1).*(px2(nj)-mu2)/C(1,2)*C(2,1)));
     Mpdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%the Sv PDF
mu1=0; mu2=0;
C=Sv; D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

Svpdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2));        
     Svpdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%the eY PDF (estimated y)
ya=rya(:,ns2); %estimated y
mu1=ya(1); mu2=ya(2);
C=cY(:,:,ns2); D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

eYpdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2));        
     eYpdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%the P PDF (P(n+1))
mu1=xe1(ns2); mu2=xe2(ns2);
C=P(:,:,ns2); D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

Ppdf=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((px1(ni)-mu1)^2)/C(1,1))+(((px2(nj)-mu2).^2)/C(2,2))...
         -(((px1(ni)-mu1).*(px2(nj)-mu2)/C(1,2)*C(2,1)));
     Ppdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;


%----------------------------------------------------------------

%display

figure(1)
subplot(1,2,1)
contour(px2,px1,Swpdf); hold on;
contour(px2,px1,aPpdf);
xlabel('x2'); ylabel('x1');
title('<process noise>, <xe(n)>');
subplot(1,2,2)
contour(px2,px1,Mpdf); hold on;
xlabel('x2'); ylabel('x1');
title('<xa(n+1)>');

figure(2)
subplot(1,2,1)
contour(px2,px1,Svpdf); hold on;
contour(px2,px1,Mpdf);
xlabel('x2'); ylabel('x1');
title('<measurement noise>, xa(n+1)>');
subplot(1,2,2)
contour(px2,px1,eYpdf); hold on;
plot(rym(2,ns1),rym(1,ns1),'r*','MarkerSize',12);
xlabel('x2'); ylabel('x1');
title('<estimated y>');


figure(3)
subplot(1,3,1)
contour(px2,px1,Mpdf);
xlabel('x2'); ylabel('x1');
title('<xa(n+1)>');
subplot(1,3,2)
contour(px2,px1,eYpdf);
xlabel('x2'); ylabel('x1');
title('<estimated y>');
subplot(1,3,3)
contour(px2,px1,Ppdf);
xlabel('x2'); ylabel('x1');
title('<xe(n+1)>');


