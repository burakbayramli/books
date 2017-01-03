function CoeffQuantizeErr(b,a,maxbits,ftype,f,Fs)

%COEFFICIENT QUANTIZATION ERROR ANALYSIS

n=256;
for nbits=2:maxbits
	[B,A]=QuantizeCoeff(b,a,nbits);	% quantize all coeffs  
	[B,A] = sos2tf([B A]);	%form A(z) and B(z)
   [h,w] = freqz(B,A,n);
	amag = abs(h);		% obtain the amplitude response
	amag = amag/max(amag);	% normalized amplitude response
	dev(nbits-1,:) = RippleAtten(ftype,f,amag,n,Fs); % obtain the pass and stopband deviations      
end

fprintf('nbits\tband1\t\tband2\t\tband3\n');
fprintf('%d\t%f\t%f\t%f\n',reshape([(2:maxbits)' dev]',maxbits-1,4));

fprintf('\nfrequency response with quantization noise for desired wordlength:\n');
nbits=input('   wordlength (32 for unquantized coefficients):  ');

[B,A] = sos2tf([b a]);	%form A(z) and B(z)
freqz(B,A,n);	%compute and plot the frequency response with unquantized coefficients
hold on;
[B,A] = QuantizeCoeff(b,a,nbits);	% compute response of nbit filter  
[B,A] = sos2tf([B A]);	%form A(z) and B(z)
freqz(B,A,n);	%compute and plot the frequency response with quantized coefficients

title('Frequency Response for Desired Wordlength');
