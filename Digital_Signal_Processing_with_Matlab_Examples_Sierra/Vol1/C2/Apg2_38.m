% Generation of random data with a desired PDF
% Using numerical inversion

%first: a table with the inversion of F
M=1001;
y=0:0.001:1; %F between 0 and 1
x=zeros(1,M); 

%incremental inversion
aux=0; dax=0.001*pi;
for ni=1:M,
   while y(ni)>sin(aux),
      aux=aux+dax;
   end;
   x(ni)=aux;
end;  

%second: generate uniform random data
N=2000; %number of data
ur=rand(1,N); %uniform distribution

%third: use inversion table
z=zeros(1,N);
for nn=1:N,
   pr=1+round(ur(nn)*1000); %compute position in the table
   z(nn)=x(pr); %read output table
end;

%display histogram of generated data
hist(z,30); colormap('cool');
xlabel('z');  
title('histogram of the generated data');
