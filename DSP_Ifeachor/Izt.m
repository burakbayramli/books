function IZT
%program IZT (izt.m) is for:
%(1) computing the inverse z-transform via the power series or partial 
%	  fraction expansion method
%(2) converting a transfer function, H(z), in cascade form to an equivalent
%    transfer function in parallel, via partial fraction expansion. The 
%    basic building block is the second order biquad

IZT_Mode = 1; 	%1 - use power series to perform the IZT
					%2 - use partial fraction/residue to perform the IZT
               %3 - cascade to parallel conversion
n = 5;	% number of power series points
b1 = [1 0.481199 1]; a1 = [1  0.052921 0.83173];
b2 = [1 1.474597 1]; a2 = [1 -0.304609 0.238865];	
B = [b1; b2];	A = [a1; a2];
[b,a] = sos2tf([B A]);
if IZT_Mode==1		%compute the IZT by power series
   b = [b zeros(1,n-1)];
   [h,r] = deconv(b,a);	%perform long division
   disp('The result of the inverse Z-transform by power series is:'); disp(h);
else 
	[res,poles,rem] = residuez(b,a);
   disp('The poles of the transform function are:'); disp(poles');
   disp('The partial fraction coefficients are:'); disp(res');
   if IZT_Mode==3
      i = 1 ;
      for j=1:size(B,1)
      	[b,a] = residuez(res(i:i+1),poles(i:i+1), 0);
   		fprintf('Numerator and Denominator for stage%d:\n',j);disp(b);disp(a);
         i = i + 2;
      end
   end
end

