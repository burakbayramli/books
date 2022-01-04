

function zz = wwprime(t,ww)

 global N kvec symbol
  % convert a real 2N column vector into a complex N column vector.
 w = ww(1:N,1) +i*ww(N+1:2*N,1);  

 uhat = w.*exp(-t*symbol);
 u_xhat = i*kvec.*uhat;

 u = ifft(uhat)*N;
 u_x = ifft(u_xhat)*N;

 v = u.*u_x;
 vhat = fft(v)/N;

 z = -exp(t*symbol).*vhat;
 % convert the complex column N vector back to a real 2N column vector

 zz(1:N,1) = real(z); zz(N+1:2*N,1) = imag(z);

