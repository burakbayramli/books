%4.6  circdeconvcode.m

bhat = fft(b); chat = fft(c); uhat = bhat./chat; u = ifft(uhat)
