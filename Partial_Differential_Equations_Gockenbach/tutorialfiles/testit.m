clear
syms ii h x real
phi1=(x-(ii-1)*h)/h;
phi2=-(x-(ii+1)*h)/h;
f=x^2;
n=10;
h=1/n;
F=zeros(n-1,1);
for ii=1:n-1
   F(ii)=int(subs(phi1)*f,x,ii*h-h,ii*h)+int(subs(phi2)*f,x,ii*h,ii*h+h);
end
F
