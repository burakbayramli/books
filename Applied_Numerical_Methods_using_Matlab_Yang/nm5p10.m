%nm5p10.m: plots the probability of bit error versus SNRbdB
fs='Q(-sqrt(2)*x-sqrt(b*SNR)).^(2^b-1)';
Q=inline('erfc(x/sqrt(2))/2','x');
f=inline(fs,'x','SNR','b');
fex2=inline([fs '.*exp(-x.*x)'],'x','SNR','b');
SNRdB=0:10; tol=1e-4;  % SNR[dB] and tolerance used for 'quad'
for b=1:4
  tmp=2^(b-1)/(2^b-1); spi=sqrt(pi);
  for i=1:length(SNRdB),
    SNR=10^(SNRdB(i)/10);
    Pe(i)=tmp*(1-Gauss_Hermite(f,10,SNR,b)/spi);
    Pe1(i)=tmp*(1-quad(fex2,-10,10,tol,[],SNR,b)/spi);
    Pe2(i)=tmp*(1-?????????????????????????????????)/spi);
  end
  semilogy(SNRdB,Pe,'ko',SNRdB,Pe1,'b+:',SNRdB,Pe2,'r.-'), hold on
end  
