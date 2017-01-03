function [B,A]=QuantizeCoeff(b,a,nbits)

if(nbits==32)
   B = b; A = a;
   return;
end
ip2=2^(nbits-1);
B = floor(sign(b).*(abs(b)*ip2+0.5))/ip2;
A = floor(sign(a).*(abs(a)*ip2+0.5))/ip2;
