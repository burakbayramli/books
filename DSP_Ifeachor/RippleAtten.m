function dev = RippleAtten(ftype,f,amag,n,Fs) 

m=floor(2*n*f/Fs);
if ftype==0         % LPF: obtain passband ripple and stopband attenuation
	dev(1)=20.0*log10(1.0+max(amag(m(1)+2:m(2)))-min(amag(m(1)+2:m(2))));
	dev(2)=-20.0*log10(max(amag(m(3)+2:m(4))));
	dev(3)=0.0;
elseif ftype==1   	% HPF: obtain passband and stopband attenuation
	dev(1)=-20.0*log10(max(amag(m(1)+2:m(2))));
	dev(2)=20.0*log10(1.0+max(amag(m(3)+2:m(4)))-min(amag(m(3)+2:m(4))));
	dev3=0.0;
elseif ftype==2 	% bandpass filter  
	dev(1)=-20.0*log10(max(amag(m(1)+2:m(2))));
	dev(2)=20.0*log10(1.0+max(amag(m(3)+2:m(4)))-min(amag(m(3)+2:m(4))));
	dev(3)=-20.0*log10(max(amag(m(5)+2:m(6))));
elseif ftype==3		%BSF: obtain passband and stopband attenuation
	dev(1)=20.0*log10(1.0+max(amag(m(1)+2:m(2)))-min(amag(m(1)+2:m(2))));
	dev(2)=-20.0*log10(max(amag(m(3)+2:m(4))));
	dev(3)=20.0*log10(1.0+max(amag(m(5)+2:m(6)))-min(amag(m(5)+2:m(6))));
end