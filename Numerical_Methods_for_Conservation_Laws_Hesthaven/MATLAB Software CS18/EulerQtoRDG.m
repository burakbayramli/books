function [qc,R] = EulerQtoRDG(q,gamma,V,iV);
% function [qc,R] = EulerQtoRDG(q,gamma,V,iV);
% Purpose: Compute characteristic values from conserved values with
% transformation based on cell average in DG formulation.
dim = size(q); m = dim(1)-1; N = dim(2); R = zeros(m+1,N,3); qc = zeros(3,N);

% Extract conserved values and compute cell averages
r = q(:,:,1); mu = q(:,:,2); E = q(:,:,3);
rh = iV*r;  rh(2:(m+1),:)=0; ra = V*rh; qc(1,:) = ra(1,:);
muh = iV*mu; muh(2:(m+1),:)=0; mua = V*muh; qc(2,:) = mua(1,:);
Eh = iV*E;  Eh(2:(m+1),:)=0; Ea = V*Eh; qc(3,:) = Ea(1,:);

% Compute characteristic values
for i=1:N
    [S, iS, Lam] = EulerChar([qc(1,i) qc(2,i) qc(3,i)],gamma);
    qh = [r(:,i) mu(:,i) E(:,i)]; Ch = (iS*qh')';
    R(:,i,1) = Ch(:,1); R(:,i,2) = Ch(:,2); R(:,i,3) = Ch(:,3);
end
return