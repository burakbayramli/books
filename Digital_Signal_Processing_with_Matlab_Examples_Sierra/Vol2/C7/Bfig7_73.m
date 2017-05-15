% Stochastic process illustration
% separated curves

t=0:0.02:10;
N=length(t);
X=0.17*randn(8,N);
%display
figure(1)
plot([0 10],[0 0],'k'); hold on
plot([0 0],[0 9],'k');
for nn=1:8,
   plot(t,nn+X(nn,:),'k');
end;  
plot([6 6],[0 9],'m');
title('Stochastic process example')
xlabel('t');
set(gca,'YtickLabel',{'0','0','0','0','0','0','0','0','0'});
