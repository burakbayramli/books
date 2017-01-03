function v=e_decode2(yc,K,Nsym,Nsh,Nrem)
%v=e_decode2(yc,K,Nsym,Nsh,Nrem)
%
%Decodes integer vector x encoded with e_encode2.
%
%Inputs: yc    =code vector produced by e_encode2.
%        K     =length(v).
%        Nsym  =# symobls in ms 8 bits of v. Must be >=2 and <=2^8.
%        Nsh   =# bits shifted out of v to get remainders
%        Nrem  =# bits in each of the K (packed) remainders attached to yc.
%
%Output: v     =decoded integer vector with min(v)=0.
%
%See also: e_encode2, a_encode_2, a_decode_2, a_encode, a_decode

if Nrem==0,                             % 8-bit integers
    v=a_decode2(yc,Nsym,K);
else                                    %9-12 bits
    M=ceil(Nrem*K/8);                   %M =# bytes containing remainders
    ya=yc(1:M);                         %ya contains the packed remainders
    res=unpack_data2(ya,Nrem,K);        %res =remainder vector unpacked
    yb=yc(M+1:length(yc));              %yb =a-encoded ms 8 bits of y
    ms8=a_decode2(yb,Nsym,K);           %ms8 =vector with ms 8 bits of y
    v=ms8*2^Nsh+res;
end