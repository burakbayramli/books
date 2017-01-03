function y=fp_bits(x,a,Pe)
if nargin<3, Pe=1e-4;
  if nargin<2, a=[64 64 64 64]; end
end  
N=128; N0=1;
x14=x(1:4);
y=(2.^x14.*(log(2)*x14-1)+1)*N0/3*2*erfcinv(Pe/2).^2-x(5);
y(5)=sum(a./x14)-N;