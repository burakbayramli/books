% Gauss Process samples

% Build a kernel matrix
sigmaf=1.2; 
gammaf=0.9; qf=2*(gammaf^2);
sigman=0.1;
x=-2:0.02:2; L=length(x);
K=zeros(L,L);

for i=1:L,
    for j=i:L,
        nse=(sigman^2)*(x(i)==x(j)); %noise term
        K(i,j)=((sigmaf^2)*exp(-((x(i)-x(j))^2)/qf))+nse;
        K(j,i)=K(i,j);
    end;
end;

% prepare for sampling
[V,D]= eig(K);
A=V*sqrt(D);

% take 9 samples
rv=zeros(L,9);
for nn=1:9, 
  rv(:,nn)=A*randn(L,1);
end;  

figure(1)
subplot(1,2,1)
imagesc(K);
title('Kernel matrix');
subplot(1,2,2)
plot(x,rv);
title('Nine samples of the Gaussian Process');
xlabel('x'); ylabel('y');
 
