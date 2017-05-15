% ISTA example
% 1D sparse signal recovery
 
% original sparse signal:
x=zeros(1,100);
x(8)=1.3; x(22)=1.9; x(37)=0.7; x(51)=0.3; x(66)=1.7; x(85)=0.9;
 
% the signal is observed through a filter
h=[1,2,3,4,5,4,3,2,1]/25; %filter impulse response
y=conv(h,x); L=length(y);
A=convmtx(h',100); %convolution matrix
% gaussian noise is added
y=y+0.06*randn(1,L);
y=y'; %column
 
% ISTA algorithm
nit=300; %number of iterations
lambda=0.1; %parameter
J=zeros(1,nit); %objective function
z=0*A'*y; %initialize z, the recovered signal
T=lambda/2; %threshold
 
for k=1:nit,
    q=A*z;
    J(k)=sum(abs(q(:)-y(:)).^2) + lambda*sum(abs(z(:)));
    p=z+(A'*(y-q)); 
    %soft thresholding (component-wise)
    for n=1:100,
     if abs(p(n)) <= T,
        z(n)=0;
     else
        if p(n)>T,
            z(n)=p(n)-T;
        else
            z(n)=p(n)+T;
        end;
     end;
    end;   
end;
  
figure(1)
subplot (3,1,1)
 [i,j,v]=find(x);
 stem(j,v,'kx'); hold on;
 axis([0 100 -0.1 2]);
 plot([0 100],[0 0],'k');
 title('original signal');
subplot(3,1,2)
 stem(y,'kx'); hold on;
 axis([0 L -0.2 0.5]);
 plot([0 L],[0 0],'k');
 title('observed signal');
subplot(3,1,3)
 stem(z,'kx');
 axis([0 100 -0.1 2]);
 title('recovered signal, using ISTA')
 
 figure(2)
 plot(J,'k')
 title('Evolution of objective function')

