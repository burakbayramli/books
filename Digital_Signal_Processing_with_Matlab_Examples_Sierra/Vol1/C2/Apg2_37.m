% Numerical inversion of a function
% example of F=sin(x), in the growing interval

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

plot(y,asin(y),'gx'); hold on; %analytical inversion
plot(y,x,'k'); %result of numerical inversion
xlabel('y'); ylabel('x'); 
title('comparison of numerical and analytical inversion of F');
