% Bayesian prediction/interpolation example

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

% new datum
nz=[5;1]; 

% PDF of predicted ny
ny=(nz'*D*z'*y)/(std^2);
rstd=(nz'*D*nz)+(std^2);

% Points of the PDF of predicted ny
x1=0:0.01:4;
L=length(x1);
K=1/(rstd*sqrt(2*pi)); Q=0.5;
ypdf=zeros(L,1); %space for the PDF
for ni=1:L,  
     aux=((x1(ni)-ny)^2)/(rstd^2);         
     ypdf(ni,1)= K*exp(-Q*aux);
end;

% display ------------------
figure(1)
plot(x,y,'r*'); hold on;
bx0=0; by0=rb;
bxf=10; byf=rmu*bxf+rb;
plot([bx0 bxf],[by0 byf],'k-');
plot(nz(1),ny,'bd','MarkerSize',10); %the predicted point
title('Bayesian prediction');
xlabel('x'); ylabel('y');

figure(2)
for ni=10:L-1, 
    plot([x1(ni) x1(ni+1)],[ypdf(ni) ypdf(ni+1)],'b'); hold on;
end;    
title('PDF of the predicted point');
xlabel('predicted y value')

        



