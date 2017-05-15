% Example of OMP
clear all;

% random A and b
N=100;
A=rand(N,2*N);
b=rand(N,1);

M=1; %measure of the residual
hM=zeros(200,1);

% OMP algorithm
Aj=[];
R=b; %residual
n=0; %counter

while M>0.1, %(edit this line)
   [v,ix]=max(abs(A'*R));
   Aj(:,ix)=A(:,ix);
   A(:,ix)=0;
   %x=Aj\b;
   x=pinv(Aj)*b;
   R=b-Aj*x;
   M=norm(R);
   n=n+1; hM(n)=M; %keep a record of residuals
end;

%display
figure(1)
stem(x,'k');
title('sparse solution');
axis([0 200 -0.6 0.6]);

figure(2)
plot(hM(1:n),'k');
title('evolution of the residual norm');
xlabel('iteration number');
   


