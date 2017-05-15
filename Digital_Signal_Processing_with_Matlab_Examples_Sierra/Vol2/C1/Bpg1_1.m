% Values of WN(kn)
k=1; %the index of Yk (please edit as you wish)
N=5; %number of samples
WN=zeros(1,N); %space for the samples
%taking the samples
for nn=1:N,
   WN(nn)=exp((-i*((2*pi)*(nn-1)*k))/N);
end; 

%display on a circle
figure(1)
k=1:(1/100):100; t=2*pi*k;
x=zeros(1,length(t)); y=zeros(1,length(t)); 
x=cos(t); y=sin(t);
plot([-1.2 1.2],[0 0],'g'); hold on; %x axis
plot([0 0],[-1.2 1.2],'g'); %y axis
plot(x,y,'b'); %the circle
for nn=1:N,   
   plot([0 real(WN(nn))],[0 imag(WN(nn))],'k');
   plot(real(WN(nn)),imag(WN(nn)),'rd');
end;
title('Values of WN(kn) with k=constant, n=0..N-1');
WN
