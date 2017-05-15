% Deterministic state evolution
%example of state dynamics model
% xn=0.6 xo + 0.7 u
%initial state
X0=0; 
Ns=10; %number of samples along time
X=zeros(1,Ns); %reserve space

u=1; %step input

X(1)=X0;
for nn=2:Ns,
   X(nn)=0.6*X(nn-1)+ 0.7 *u;
end;

figure(1)
plot(X,'k*-'); hold on;
xlabel('samples'); ylabel('state');
title('Step response');

