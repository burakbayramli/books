%nm1p19: Quantization Error
x=2-2^-50;
for n=1:2^3
   x=x+2^-52; fprintf('%20.18E\n',x)
end
