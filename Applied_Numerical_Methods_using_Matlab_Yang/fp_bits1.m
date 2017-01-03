function y=fp_bits1(x,a,N,Pe)
N0=1; y=sum((2.^x-1)*N0/3*2*erfcinv(Pe/2).^2.*a./x);
