%rbm.m
n=10^4; h=10^(-3); t=h.*(0:n); mu=-1; 
X=zeros(1,n+1); M= X; B=X; 
B(1) =3; X(1) = 3;
for k= 2:n+1
  Y= sqrt(h)*randn; U=rand(1); 
  B(k) = B(k-1) + mu*h - Y;
  M=(Y + sqrt(Y^2-2*h*log(U)))/2; 
  X(k)=max(M-Y, X(k-1)+h*mu-Y);
end
subplot(2,1,1)
plot(t,X,'k-');
subplot(2,1,2)
plot(t,X-B,'k-');

