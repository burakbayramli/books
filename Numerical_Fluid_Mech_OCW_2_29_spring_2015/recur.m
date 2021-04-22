clear
N=20; sum=0; sumr=0;
b=1; c=1; x=0.5;
xn=1;
% Number of significant digits in computations
dig=2;
ndiv=10;
for i=1:N
  a1=sin(pi/2-pi/(ndiv*i));
  a2=-cos(pi/(ndiv*(i+1)));
% Full matlab precision
  xn=xn*x;
  addr=xn+b*a1;
  addr=addr+c*a2;
  ar(i)=addr;
  sumr=sumr+addr;
  z(i)=sumr;
% additions with dig significant digits
  add=radd(xn,b*a1,dig);
  add=radd(add,c*a2,dig);
% add=radd(b*a1,c*a2,dig);
% add=radd(add,xn,dig);
  a(i)=add;
  sum=radd(sum,add,dig);
  y(i)=sum;
end
sumr
'      i       delta      Sum    delta(approx) Sum(approx)'
res=[[1:1:N]' ar' z' a' y']

hold off
a=plot(y,'b'); set(a,'LineWidth',2);
hold on
a=plot(z,'r'); set(a,'LineWidth',2);
a=plot(abs(z-y)./z,'g'); set(a,'LineWidth',2);
legend([ num2str(dig) ' digits'],'Exact','Error');
grid on


