function z = ola(x,y,N);
% Synopsis: z = ola(x,y,N).
% Computes the convolution z = x*y by the overlap-add method.
% Input parameters:
% x: the long input sequence
% y: the short  input sequence
% N: length of the FFT.
% Output parameters:
% z: the output sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N2 = length(y); lx = length(x); y = reshape(y,1,N2);
x = reshape(x,1,lx); z = zeros(1,N2+lx-1);
lz = length(z); rflag = 0; N1 = N-N2+1;
if (any(imag(x)) | any(imag(y))), rflag = 1; end
nframe = floor(lx/N1); ltail = lx - N1*nframe;
Y = fft([y, zeros(1,N-N2)]);
for k = 1:nframe,
   nst = (k-1)*N1;
   temp = ifft(fft([x(1,nst+1:nst+N1), zeros(1,N-N1)]).*Y);
   if (rflag), temp = real(temp); end
   z(1,nst+1:nst+N) = z(1,nst+1:nst+N) + temp;
end
if (ltail > 0),
   nst = nframe*N1+1; temp = [x(1,nst:lx), zeros(1,N-ltail)];
   temp = ifft(fft(temp).*Y);
   if (rflag), temp = real(temp); end
   z(1,nst:lz) = z(1,nst:lz) + temp(1,1:N2+ltail-1);
end

