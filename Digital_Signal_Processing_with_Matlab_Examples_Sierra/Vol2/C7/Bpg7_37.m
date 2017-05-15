% Bayesian regression example

% generate a data set
m=0.4; %slope
b=0.6; %intercept
N=25; %number of data points
x=10*rand(N,1);
std=0.2;
nse=normrnd(0,std,N,1); %noise
y=m*x+b+nse;
z=cat(2,x,ones(N,1)); %add second column of 1's for the intercept

% PDF of line parameters
D=zeros(2,1);
gamma=[0.2 0;0 0.6]; %(edit the diagonal numbers)
aux1=(z'*z)/(std^2); aux2=inv(gamma^2);
D=inv(aux1+aux2);
rpar=(D*z'*y)/(std^2);
rmu=rpar(1); rb=rpar(2);
rstd=D;

% Points of the PDF of line parameters
x1=0:0.02:2;
x2=0:0.02:2;
L=length(x1);
dd=det(rstd);
K=1/(2*pi*sqrt(dd)); Q=1/2;
ypdf=zeros(L,L); %space for the PDF
for ni=1:L,
   for nj=1:L,
     aux=(((x1(ni)-rmu)^2)/rstd(1,1))+(((x2(nj)-rb)^2)/rstd(2,2));         
     ypdf(ni,nj)= K*exp(-Q*aux);
  end;
end;

% display ------------------
figure(1)
contour(x1,x2,ypdf);
axis([0 1 0 0.8]);
grid;
title('PDF of line parameters');
xlabel('intercept'); ylabel('slope');

figure(2)
plot(x,y,'r*'); hold on;
bx0=0; by0=rb;
bxf=10; byf=rmu*bxf+rb;
plot([bx0 bxf],[by0 byf],'k');
title('Bayesian regression');
xlabel('x'); ylabel('y');
   