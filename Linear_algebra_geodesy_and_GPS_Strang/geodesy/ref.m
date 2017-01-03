function [R,pivcol] = ref(A)
%REF	Reduced Row Echelon Form.
%	   R = ref(A) uses the pivoting LU factorization computed by PLU
%	   to find the reduced row echelon form of a rectangular matrix A.

[P,L,U,pivcol] = plu(A);
% Scale rows so that pivots are one and eliminate
% nonzeros above the pivots.
R = U;
[m,n] = size(R);
for k = 1:length(pivcol);
   p = pivcol(k);
   for j = p+1:n
      R(k,j) = R(k,j)/R(k,p);
   end
   R(k,p) = 1;
   for i = 1:k-1
      for j = p+1:n
         R(i,j) = R(i,j) - R(i,p)*R(k,j);
      end
      R(i,p) = 0;
   end
end
%%%%%%%%%%%%%%%%%%%%%%% end plu.m %%%%%%%%%%%%%%%