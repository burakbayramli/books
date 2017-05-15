%Unscented Transformation example
% from polar to Cartesian coordinates
% measurement of satellite position at T0

%
%reference position at T0
r0=600;
alpha0=pi/2;
%variances
sigr=20;
siga=0.45; %radians

%------------------------------------------------------
%bivariate Gaussian PDF on the alpha-r plane
al=0.4*pi;
da=-al:(al/50):al;
pN1=length(da);
dr=-100:1:100;
pN2=length(dr);
r=r0+dr;
a=alpha0+da;

C=[siga^2 0; 0 sigr^2];
mu1=alpha0; mu2=r0;
D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

p=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((a(ni)-mu1)^2)/C(1,1))+(((r(nj)-mu2).^2)/C(2,2));        
      p(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%sigma points
mu1=alpha0; mu2=r0;
xs=zeros(2,6); %reserve space
xs0=[mu1; mu2];
xs(:,1)=xs0+[siga;0]; xs(:,2)=xs0+[0;sigr];
xs(:,3)=xs0-[siga;0]; xs(:,4)=xs0-[0;sigr];

figure(1)
contour(a,r,p'); hold on;
%sigma points
plot(xs0(1),xs0(2),'r*','MarkerSize',12); 
for nn=1:4,
   plot(xs(1,nn),xs(2,nn),'rx','MarkerSize',12);
end;   
title('Original sigma points and PDF');
xlabel('rad'); ylabel('m');

%------------------------------------------------------
%Transform PDF from alpha-r plane to x-y plane
MC=contourc(a,r,p');

auxA=1; aux0=0;
ncs=zeros(1,8); csa=zeros(8,500); csr=zeros(8,500);
for nn=1:8,
aux0=MC(2,auxA); %number of segments in contour leveln
ncs(nn)=aux0;
csa(nn,1:aux0)=MC(1,(auxA+1):auxA+aux0); %select the leveln segments
csr(nn,1:aux0)=MC(2,(auxA+1):auxA+aux0); %select the leveln segments
auxA=auxA+aux0+1;
end;

%transform sigma points
muy1=mu2*cos(mu1);
muy2=mu2*sin(mu1);
ys=zeros(2,6); %reserve space
ys0=[muy1; muy2];
for nn=1:4,
   ys(1,nn)=xs(2,nn)*cos(xs(1,nn));
   ys(2,nn)=xs(2,nn)*sin(xs(1,nn));
end;


figure(2)
for m=1:8,
   L=ncs(m);
   sm=zeros(2,L);
   for nn=1:L,
     axa=csa(m,nn); axr=csr(m,nn);
     sm(1,nn)=axr*cos(axa);
     sm(2,nn)=axr*sin(axa);
   end;   
   plot(sm(1,:),sm(2,:),'b'); hold on;
end;
%transformed sigma points
plot(ys0(1),ys0(2),'r*','MarkerSize',12); hold on;
for nn=1:4,
   plot(ys(1,nn),ys(2,nn),'rx','MarkerSize',12);
end; 
title('Propagated sigma points and PDF');
xlabel('m'); ylabel('m');

%-------------------------------------------------------
% Use sigma points

%UKF parameters (to be edited here)
N=2; %space dimension
alpha=0.55; kappa=1; beta=2;
%pre-computation of constants
lambda= ((alpha^2)*(N+kappa))-N;
aab=(1-(alpha^2)+beta); 
lN=lambda+N; LaN=lambda/lN; aaN=aab+LaN;

%mean: weighted average
ya=[0;0];
for nn=1:4,
   ya=ya+ys(:,nn);
end;
ya=ya/(2*lN);
ya=ya+(LaN*ys0);

%covariance: weighted average
aux=zeros(2,2); aux1=zeros(2,2);
for nn=1:4,
   aux=aux+(ys(:,nn)-ya(:))*(ys(:,nn)-ya(:))';
end;
aux=aux/(2*lN);
aux1=(ys0-ya)*(ys0-ya)';
Py=aux+(aaN*aux1);

%the Gaussian PDF approximation
C=Py;
mu1=ya(1); mu2=ya(2);
D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

x1=-700:10:700;
x2=0:10:900;
pN1=length(x1); pN2=length(x2);
yp=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((x1(ni)-mu1)^2)/C(1,1))+(((x2(nj)-mu2).^2)/C(2,2));        
      yp(ni,nj)= K*exp(-Q*aux1);
  end;
end;

%Set of random measurements
Np=1000; %number of random points
px=zeros(1,Np);
py=zeros(1,Np);
nr=sigr*randn(1,Np);
na=siga*randn(1,Np);
for nn=1:Np,
   r=r0+nr(nn);
   a=alpha0+na(nn);
   px(nn)=r*cos(a);
   py(nn)=r*sin(a);
end; 

xmean=sum(px/Np);
ymean=sum(py/Np);

figure(3)
plot(px,py,'g.'); hold on; %the points
contour(x1,x2,yp'); %the UT PDF approximation
plot(ya(1),ya(2),'b+', 'MarkerSize',12); %the PDF center
plot(xmean,ymean,'kx', 'MarkerSize',12); %the data mean
title('Some propagated data points, and the PDF approximation by UT');
xlabel('m'); ylabel('m');
axis([-700 700 0 800]);