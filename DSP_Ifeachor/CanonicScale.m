function s = CanonicScale(b,a,iopt,n)

% function CanonicScale computes the scale factors of a transfer
% function realized by cascaded second order canonic sections

if(iopt>=3)                  % scaling not required  
   s = ones(1,size(b,1));
   return;
else
   A = 1; B = 1;
   for i=1:size(b,1)	%loop for each stage
      A = conv(A,a(i,:));
      if i>1
         B = conv(B,b(i-1,:));
      end
      s(i) = GetScaleFactor(B,A,iopt,n);
   end
end

