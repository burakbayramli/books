% Stochastic process illustration
% overlapping curves

t=0:0.2:10;
N=length(t);
X=0.17*randn(8,N);
%display
figure(1)
plot([0 10],[0 0],'k'); hold on
plot([0 0],[-0.6 0.6],'k');
for nn=1:8,
   plot(t,X(nn,:),'k');
end;  
axis([0 10 -0.6 0.6]);
title('Stochastic process example')
xlabel('t');
