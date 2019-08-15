function [KK,MM,Q] = matrizen2(p,e,q,RC);
% bilinear parallelogram elements
% lineare boundary integrals

N = size(p,2); KK = sparse(N,N); MM = sparse(N,N);
Q = zeros(N,1);  MQ = sparse(N,N);
ecode = 0;
% -- linear parallelogram elements-----------------
for I = 1:size(q,2)
   K = q([1,2,4],I);
   X = p(1,K); Y = p(2,K);
   [KE,ME,BE,ecode] = bilin(X,Y);
   L      = q(1:4,I);
   KK(L,L) = KK(L,L) + KE;                  %ev. + RHO*ME;
   MM(L,L) = MM(L,L) + ME;
 % I
 % ecode
end
% -- Cauchy boundary conditions --------------
for I = 1:size(RC,2)
   J      = RC(1:2,I);
   [MR,BR]= ralell(p(1,J),p(2,J));
   Q(J)   = Q(J) + BR*RC(3,I);
   MQ(J,J) = MQ(J,J) + MR;
end
