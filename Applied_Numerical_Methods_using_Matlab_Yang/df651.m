function dx=df651(t,x)
dx=zeros(size(x));
dx(1)=x(2);  dx(2)=-x(2)+1;