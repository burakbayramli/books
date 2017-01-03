function H=Householder(x,k)
%Householder transform to zero out tail part starting from k+1
H=eye(N)-2*w*w'; %Householder matrix 
N= length(x);
w=zeros(N,1); 
w(k)=(x(k)+g)/c; w(k+1:N)=x(k+1:N)/c;%Eq.(P8.4-10)
tmp= sum(x(k+1:N).^2);
c=sqrt((x(k)+g)^2 +tmp); %Eq.(P8.4-11)
g=sqrt(x(k)^2+tmp); %Eq.(P8.4-9)
