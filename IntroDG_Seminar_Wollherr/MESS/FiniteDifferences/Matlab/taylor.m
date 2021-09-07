
function [out] = taylor(n)
% High-order finite difference operator based on Taylor series
% First derivative
% Assumption of x values at x+-n dx/2
% n length of operator 


ord=n;

m=zeros(ord);
s=(1:ord)*0;
s(2)=1;

for i=1:ord,
    for j=1:ord/2,
       
       if i==1, m(i,j)= 1.; end

       if i > 1, 
       m(i,j) = 1/fac(i-1) * ((2*j-1)/2)^(i-1);
       end

       m(i,ord/2+j)=m(i,j)*(-1)^(i-1);

  
    end
end

oper=inv(m)*s';
out(1:ord/2)=oper(ord/2:-1:1);
out(ord/2+1:ord)=oper(ord/2+1:ord);



