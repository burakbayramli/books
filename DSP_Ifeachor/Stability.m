function Stability(b,a,maxbits)

%function Stability computes the pole radius and angle of each second
%order section of the transfer function before(r1,angle1) and after
%(r2,angle2) the coefficient quantization for different wordlengthes

format long;
fprintf('\n\nnbits\tstage\tA1\tA2\tradius1\tangle1\tradius2\tangle2\n');
for nbits=2:maxbits
	[B,A]=QuantizeCoeff(b,a,nbits);
	for i=1:size(b,1)
		r1 = sqrt(abs(A(i,3)));
		angle1 = 180/pi*acos(A(i,2)/(-2.0*r1));
		r2 = sqrt(abs(a(i,3)));
		angle2 = 180/pi*acos(a(i,2)/(-2.0*r2));
		fprintf('%d\t%d\t%-7.4f\t%-7.4f\t%-7.4f\t%-7.2f\t%-7.4f\t%-7.2f\n',nbits,i,A(i,2),A(i,3),r1,angle1,r2,angle2);
	end
end
format;

