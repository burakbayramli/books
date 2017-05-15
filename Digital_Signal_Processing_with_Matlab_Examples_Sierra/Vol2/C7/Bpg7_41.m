% Gauss Process regression

% Generate observed data
No=20; %number of observations
xo=-2+(4*rand(No,1));
msig=0.2
yo=(2*sin(2*xo))+(msig*randn(No,1)); %suppose there is some measurement noise

% Build a kernel matrix wit the observations
sigmaf=1.2; 
gammaf=0.9; qf=2*(gammaf^2);
sigman=0.1;
K=zeros(No,No);

for i=1:No,
    for j=i:No,
        nse=(sigman^2)*(xo(i)==xo(j)); %noise term
        K(i,j)=((sigmaf^2)*exp(-((xo(i)-xo(j))^2)/qf))+nse;
        K(j,i)=K(i,j);
    end;
end;

% generate x coordinates of points to be predicted
x=-2:0.02:2; L=length(x);

% Build K** matrix for data to be predicted
Kaa=zeros(L,L);
for i=1:L,
    for j=i:L,  %(no noise term)
        Kaa(i,j)=((sigmaf^2)*exp(-((x(i)-x(j))^2)/qf));
        Kaa(j,i)=Kaa(i,j);
    end;
end;

% Build K* matrix
Ka=zeros(L,No);
for i=1:L,
    for j=1:No,
        Ka(i,j)=((sigmaf^2)*exp(-((x(i)-xo(j))^2)/qf));
    end;
end;

% Obtain mean and std
qi=inv(K);
mu=(Ka*qi)*yo;
std=Kaa-(Ka*qi*Ka');
sigma=(1/msig)*sqrt(diag(std));
    
% display 
scolor=[0.7,0.85,0.8]; hh=(mu+sigma)'; ll=(mu-sigma)';
set(fill([x,x(end:-1:1)],[hh,fliplr(ll)],scolor,'EdgeColor',scolor));
hold on;
plot(x,mu,'k'); hold on;
plot(xo,yo,'r*','MarkerSize',8);
axis([-2.1 2.1 -2.5 2.5]);
title('Gaussian Process regression');
xlabel('x');
  