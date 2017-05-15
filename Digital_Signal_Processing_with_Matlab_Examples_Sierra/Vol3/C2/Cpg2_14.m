%Perona-Malik diffusivity function and flux function
% (first alternative)
%
lambda=3;
s=0:0.1:10; %variable values
g=1./(1+(s.^2/lambda^2));
f=s.*g;

figure(1)
subplot(1,2,1);
plot(s,g,'k');
xlabel('s'); grid;
title('P-M diffusivity');
axis([0 10 0 1.2]);

subplot(1,2,2);
plot(s,f,'k');
xlabel('s'); grid;
title('Flux function');
axis([0 10 0 1.6]);







