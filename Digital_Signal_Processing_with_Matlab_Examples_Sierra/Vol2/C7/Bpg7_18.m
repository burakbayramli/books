% Laplace PDF
% compared to Gaussian PDF

x=-4:0.1:4;
yn=normpdf(x,0,1); %normal pdf
k=sqrt(2); %normalization constant
yL=(1/k)*exp(-k*abs(x)); %Laplace pdf

%display
figure(1)
plot(x,yL,'k'); hold on;
plot(x,yn,'r');

title('Laplace PDF compared with Gaussian PDF')
xlabel('x'); ylabel('y');
