function [y,Nbits]=h_encode(x)
% [y,Nbits]=h_encode(x)
% Huffman encoding using a fixed frequency table computed
% by freq(x).
%
% x     =input vector or array. The elements in x are assumed
%        to be in the range [0,xmax].
%
% y     =vector of bytes. Each byte has 8 bits of the 
%        Huffman-coded version of x.
%
% Nbits =Total number of bits in y. Let Ny = length(y).
%        Then 8*(Ny-1) < Nbits <= 8*Ny.
%
% See also h_codes, code_length, freq
Nx=length(x);
[c,len]=h_codes(x);
y=zeros(ceil(Nx*max(len)/8),1);
% i=index in y; nb=bit# (7-0).
i=0;
nb=-1;
for k=1:Nx,
   L=len(x(k)+1);
   if L>1,
      iy=sp_bits(c(x(k)+1),L);
   else
      iy=c(x(k)+1);
   end
   for n=1:L,
      if(nb<0),
         i=i+1;
         nb=7;
      end
      y(i)=y(i)+iy(n)*2^nb;
      nb=nb-1;
   end
end
Nbits=sum(len(x+1));
y=y(1:i);