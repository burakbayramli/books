function out = simplelqr(q,r)
% simplelqr(q,r)
%A demo to show simple scalar LQR

clf;
numpts = 10;

a=1.1;
b=-1.0;
x0=10;

I=eye(numpts);
O=zeros(numpts);

J(1,:) = zeros(1,numpts);
for i=2:numpts,
  J(i,:)=I(i-1,:);
end 
  
Q=[q*I,O;O,r*I];
A=[I,-b*I]+[-a*J,O];
B=zeros(numpts,1);
B(1)=a*x0;

Z=inv(Q)*A'*inv(A*inv(Q)*A')*B;

x=Z(1:numpts);
u=Z(numpts+1:2*numpts);

hold on;

subplot(2,1,1);
plot(x);
xlabel('Time')
ylabel('State x')

subplot(2,1,2);
plot(u);
xlabel('Time')
ylabel('Control u')

hold off;
