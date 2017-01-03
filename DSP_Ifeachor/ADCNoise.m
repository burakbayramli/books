function ADCNoiseGain=ADCNoise(b,a,n,FM)

[B,A] = sos2tf([b a]);	%form A(z) and B(z)
[h,t] = impz(B,A,n);
ADCNoiseGain = sum(h.^2)/12.0;

%Output the results
fprintf('ADC noise gain is %f\n\n',ADCNoiseGain);
if FM~=1
	fprintf('ADC noise is %g^2*%g*q^2\n',[FM ADCNoiseGain]);
else
	fprintf('ADC noise is %g*q^2\n',ADCNoiseGain);
end
