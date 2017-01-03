function RoundoffNoise(b,a,n,nbits,iopt)

% function RoundoffNoise computes the scale factors and roundoff 
% noise of the transfer function realized by the cascaded second
% order canonic sections

if(nbits<2)
	nbits=2;
elseif(nbits>28)
   nbits=32;
end
norm = CanonicScale(b,a,iopt,n);	% obtain scale factors 
temp = norm./[norm(2:size(b,1)) 1];
for i=1:size(b,1)	% scale the coefficients
	B(i,:)=b(i,:)*temp(i);
end
[B,A] = QuantizeCoeff(B,a,nbits);
norm_square = NormSquare(B,A);   % norm_square(i)=||Hi(z)||2 squared
roundoff_noise_gain = 3*norm_square(1);
for i=2:size(b,1)
	roundoff_noise_gain = roundoff_noise_gain+5*norm_square(i);
end
roundoff_noise_gain = (roundoff_noise_gain + 3)/12;

fprintf('Scale factors: '); disp(norm);
fprintf('Norm squared values for Fi(z): '); disp(norm_square);
fprintf('roundoff noise: %g*q^2\n',roundoff_noise_gain);