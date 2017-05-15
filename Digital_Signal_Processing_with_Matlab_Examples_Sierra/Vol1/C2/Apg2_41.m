% Two overlapped PDFs

x=0:0.01:2.5;

%densities
alpha=5; beta=3;
Apdf=betapdf(x,alpha, beta); %beta PDF
mu=1.3; sigma=0.3;
Bpdf=normpdf(x,mu,sigma); %normal pdf

%product of PDFs at intersection zone
piz=Apdf(50:100).*Bpdf(50:100);

%display
figure(1)
plot(x,Apdf,'r'); hold on;
plot(x,Bpdf,'b');
title('Two random variables A and B: their PDFs')
xlabel('x'); ylabel('y');

figure(2)
subplot(2,1,1)
plot(x,Bpdf,'b'); hold on;
plot(x(50:100),Apdf(50:100),'r')
plot([x(50) x(50)],[0 Apdf(50)],'r--');
plot(x(50:100),piz,'k');
title('f(A|B) * f(B)');

subplot(2,1,2)
plot(x,Apdf,'r'); hold on;
plot(x(50:100),Bpdf(50:100),'b')
plot([x(100) x(100)],[0 Bpdf(100)],'b--');
plot(x(50:100),piz,'k');
title('f(B|A) * f(A)');

figure(3)
join=Apdf.*Bpdf;
plot(x(50:100),join(50:100),'k'); hold on;
plot(x,Apdf,'r'); hold on;
plot(x,Bpdf,'b');
title('f(A,B)=f(A)*f(B)') 
