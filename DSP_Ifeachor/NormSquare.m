function norm_square = NormSquare(b,a)

% function NormSquare computes the scale norm squares of a transfer
% function realized by cascaded second order canonic sections

A = 1; B = 1;
for i=1:size(b,1)	%loop for each stage
   A = conv(A,a(i,:));
   B = conv(B,b(i,:));
end
for i=1:size(b,1)	%loop for each stage
   if i>1
      A = deconv(A,a(i-1,:));
      B = deconv(B,b(i-1,:));
   end
   s(i) = GetScaleFactor(B,A,1,300); % get L2 norm from 300 points
end
norm_square = s.*s;