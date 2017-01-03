function p=Lgndrp(N) %Legendre polynomial
if N<=0, p=1; %n*Ln(t)=(2n-1)t Ln-1(t)-(n-1)Ln-2(t)
 elseif N==1, p=[1 0];
 else p=((2*N-1)*[Lgndrp(N-1) 0]-(N-1)*[0 0 Lgndrp(N-2)])/N;
end