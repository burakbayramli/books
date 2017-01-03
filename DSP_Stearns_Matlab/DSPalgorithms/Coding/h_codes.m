function [hcode,L]=h_codes(x)
% [hcode,L]=h_codes(x)
% x     =input vector or array. The elements in x are assumed
%        to be in the range [0,xmax].
% hcode =col. vector of Huffman codes, h(1:xmax+1).
% L     =col. vector such that L(j)=length in bits of hcode(j).
%
% See also h_encode, code_length, freq
L=code_length(x);
Ncodes=length(L);
% test_Ncodes=Ncodes
maxL=max(L);
% Compute NL = # codes of length 1,2,...,maxL.
NL=zeros(maxL,1);
for j=1:Ncodes;
   index=L(j);
   NL(index)=NL(index)+1;
end
% test_NL=NL'
% Compute minh=min. code for each length.
minh=zeros(maxL,1);
for j=1:maxL-1;
   minh(j+1)=2*(minh(j)+NL(j));
end
% test_minh=minh'
% Compute all the codes.
hcode=[1:Ncodes]';
for n=1:Ncodes;
   j=L(n);
   NL(j)=NL(j)-1;
   hcode(n)=minh(j)+NL(j);
end
% test_hcode=hcode'
% test_L=L'