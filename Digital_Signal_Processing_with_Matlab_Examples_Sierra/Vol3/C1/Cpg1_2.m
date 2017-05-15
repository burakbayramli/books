%K in function of sigmay/sigmax
%
%values of sigmay/sigmax 
r=0.1:0.1:2;
%reserve space
K=zeros(1,length(r));

figure(1)
K=1./(1+r.^2); %vectorized code
plot(r,K,'k');

title('K vs. sigy/sigx');
xlabel('sigy/sigx'); ylabel('K');
axis([0 2.1 0 1.1]);
