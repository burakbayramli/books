function [yc,K,Nsym,Nsh,Nrem]=e_encode2(v)
%[yc,K,Nsym,Nsh,Nrem]=e_encode2(v)
%
%Adaptive arithmetic encoding of integer vector v with min(v)=0
% and max(v)<2^16.
%When max(v) is <2^8, e_encode2 is the same as a_encode2.
%When max(v) is >=2^8, the most-significant 8 bits of v are encoded
% using a_encode2 and the residues are packed as white noise.
%
%Input:   v  =integer vector with min(v)=0 and max(v)<2^16.
%
%Outputs: yc    =unint8 encoded vector
%         K     =length(v)
%         Nsym  =# symobls in ms 8 bits of v. Must be >=2 and <=2^8
%         Nsh   =# bits shifted out of v to get remainders
%         Nrem  =# bits in each of the K packed remainders attached to yc.
%
%See also: e_dencode2, a_encode_2, a_decode_2, a_encode, a_decode

%check the input
v=double(v(:));                         %assure v is a f.p. col. vector
if min(v)~=0 | max(abs(v-fix(v)))~=0 | max(v)>=2^16,
    error('v must be integers with min(v)=0 and max(v)<2^16');
end

Nb=ceil(log2(max(v)+1));                %Nb =#bits needed for [0,max(v)]
Nsh=Nb-8;
if Nb<=8,                               %v elements are 8 bits.
    [yc,Nsym,K]=a_encode2(v);           %K =length(v); Nsym =#symbols is v
    Nsh=0;
    Nrem=0;
else                                    %v elements are >8 bits.
    sh=2^(Nsh);                         %sh used to shift out remainder
    res=mod(v,sh);                      %res =rem. vector of length K
    [ya,Nrem]=pack_data2(res);          %length(ya) =ceil(Nrem*K/8) bytes
    [yb,Nsym,K]=a_encode2(fix(v/sh));   %yb =encoded ms 8 of 12 bits
    yc=[ya;yb];                         %length(ya) is computable
end