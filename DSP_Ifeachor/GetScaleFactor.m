function s = GetScaleFactor(b,a,iopt,n)
 
% function GetScaleFactor computes the scale factors for L1, L2, and 
% Loo forms from a transfer function described by coefficients a and b

if (iopt==0|iopt==1)
	[h,t] = impz(b,a,n); %impulse response
	if (iopt)
      s = sqrt(sum(h.^2));
   else
      s = sum(abs(h));
   end
elseif (iopt==2)
	[h,w] = freqz(b,a,n); % frequency response
	s = max(abs(h));
else
   s = 1;	% otherwise no scale
end

