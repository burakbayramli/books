function [FB,DCT] = init_filtbank(Fs,fft_len)
% function [FB,DCT] = init_filtbank(Fs,fft_len)
%	Initialize mel scale filterbank.
%
%	Fs = data sampling rate to determine nf
%	fft_len = fft length of spectra
%
%	FB = nf x fft_len/2+1 matrix of triangular 
%	     mel scale filters
%	DCT = f x nf DCT matrix
%

klo = 0;
khi = fft_len/2;
fmax = (khi-klo+1)*Fs/fft_len;
finc = fmax / (khi-klo+1);

fc = [0:10]*100;		% 100 Hz 

f = 1000;
nf = 11;
while 1				% 10% spacing
	f = f*1.1;
        fc(nf+1) = f;
	nf = nf + 1;
	if (f >= fmax), break, end
end

nf = nf - 2;
if ( (fc(nf+1)+fc(nf+2))/2 > fmax ), nf = nf - 1, end

for i=1:nf,

	FB(i,:) = zeros(1,khi-klo+1);
        area = 0.0;
	j = 1;

	for f=0.0:finc:fmax

	    test = (f > fc(i)) & (f < fc(i+2));
            if test

            	if (f < fc(i+1)) 
                	FB(i,j) = (f - fc(i)) / (fc(i+1) - fc(i));
		else
                	FB(i,j) = (fc(i+2) - f) / (fc(i+2) - fc(i+1));
            	end 

	    end

	    j = j + 1;
	    if (j > khi-klo+1), break, end

        end % loop f

	FB(i,:) = FB(i,:) / sum(FB(i,:));

end % loop i

% 
% Create cos transform matrix
%
c = [0:nf-1];
r = (c + 0.5)*pi/nf;
DCT = cos(c'*r)/nf;

