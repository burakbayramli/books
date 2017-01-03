function s = DirectScale(b,a,iopt,n)

% function DirectScale computes the scale factors for a transfer
% function realized by cascaded second order sections in direct structure

if(iopt>=3)	% scaling not required  
   s = ones(1,size(b,1));
   return;
else
   A = 1; B = 1;
   for i=1:size(b,1)	%loop for each stage
      A = conv(A,a(i,:));
      B = conv(B,b(i,:));
      s(i) = GetScaleFactor(B,A,iopt,n);
   end
end