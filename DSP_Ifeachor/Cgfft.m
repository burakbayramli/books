function x=cgfft(x,npt,direction)
										
%   Function computes the DFT of a sequence using radix2 FFT                	
%   with constant geometry                                                     

% in-place bit reverse shuffling of data 

j=2;
for n=2:npt
	if n<j
		t = x(j-1);
		x(j-1) = x(n-1);
		x(n-1) = t; 
	end
	k = npt/2;
	while k<j-1
		j = j-k;
		k = k/2;
	end
	j = j+k;
end
m = log2(npt); % calculate the number of stages: m=log2(npt) 

w = exp(direction*2*pi*(0:npt/2-1)/npt*i); % pre-compute the twiddle factors

% perform the FFT computation for each stage 

for d=1:m
	n = 1;
	dk = 2^(d-1);
	dr = npt/(2^d);
	for k=0:dk-1
		p = (k*npt)/(2^d)+1;
		for r=1:dr
			if rem(d,2)~=0
				t = x(2*n)*conj(w(p));	
				x1(n) = x(2*n-1)+t;
				x1(npt/2+n) = x(2*n-1)-t;	
			else 
				t = x1(2*n)*conj(w(p));
				x(n) = x1(2*n-1)+t;
				x(npt/2+n) = x1(2*n-1)-t;
			end
			n = n+1;
		end
	end
end
if rem(m,2)~=0
	x = x1;
end

%If inverse fft is desired divide each coefficient by npt 
if direction==-1
	x = x /npt;
end