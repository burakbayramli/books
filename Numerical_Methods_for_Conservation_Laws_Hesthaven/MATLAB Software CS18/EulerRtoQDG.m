function [q] = EulerRtoQDG(R,qc,gamma,V,iV);
% function [q] = EulerRtoQ(R,qc,gamma,V,iV);
% Purpose: Compute conserved values from characteristic values with
% transformation based on cell average in DG formulation.
dim = size(R); m = dim(1)-1; N = dim(2); q = zeros(m+1,N,3);

% Compute conserved values
for i=1:N
   [S, iS, Lam] = EulerChar([qc(1,i) qc(2,i) qc(3,i)],gamma);
   qh = [R(:,i,1) R(:,i,2) R(:,i,3)]; Ch = (S*qh')';
   q(:,i,1) = Ch(:,1); q(:,i,2) = Ch(:,2); q(:,i,3) = Ch(:,3);
end
return