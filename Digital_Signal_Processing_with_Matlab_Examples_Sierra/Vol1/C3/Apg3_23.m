% Propagation of Gaussian state noise: details of states X2 and X3
%example of state dynamics model
% xn=0.6 xo + 0.5 w;

%autonomous behavior with  process noise---------------
Ns=10; %number of samples along time
X=zeros(5000,Ns); %reserve space

for np=1:5000,
X(np,1)=5; %initial state
for nn=2:Ns,
   X(np,nn)= 0.6*X(np,nn-1)+ 0.5*randn(1); %with process noise
end;
end;

[Y2,V2]=hist(X(:,2),40);
[Y3,V3]=hist(X(:,3),40);
R=0.5*randn(5000,1);
[YR,VR]=hist(R,40);

figure(1)
plot(V2,Y2,'k'); hold on;
plot(VR,YR,'b');
plot(V3,Y3,'r')
title('transition from X(2) to X(3)');