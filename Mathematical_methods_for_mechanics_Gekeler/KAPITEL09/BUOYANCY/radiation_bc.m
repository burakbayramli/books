function B = radiation_bc(p,p1,TT,RCT,DT,KAPPA,T_AIR)
% specified heat radiation on boundary 

X = p(1,:); Y = p(2,:);
N = size(p,2) + size(p1,2);

B = zeros(3*N,1);
if ~isempty(RCT)
   B = zeros(N,1);
   for I = 1:size(RCT,2)
      K       = RCT(1:3,I);
      [ME,BE] = fem_raqell(X,Y);
      B(K)    = B(K) - DT*KAPPA*(ME*TT(K) - T_AIR*BE);
   end
   B = [zeros(2*N,1);B];   
end
