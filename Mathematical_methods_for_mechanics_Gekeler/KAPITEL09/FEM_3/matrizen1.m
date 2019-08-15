function [KK,MM,Q] = matrizen1(p,e,t,RDZC);
% lineare Dreieckelemente
% lineare Randintegrale

N = size(p,2); KK = sparse(N,N); MM = sparse(N,N);
Q = zeros(N,1); %MQ = sparse(N,N);
ecode      = 0;
for I = 1:size(t,2)
    J = t(1:3,I);
   [KE,ME,BE,ecode] = drlell(p(1,J),p(2,J));
   KK(J,J) = KK(J,J) + KE;                  %ev. + RHO*ME;
   MM(J,J) = MM(J,J) + ME;
end
for I = 1:size(RDZC,2)
   J       = RDZC(1:2,I);
   [MR,BR] = ralell(p(1,J),p(2,J));
   Q(J)    = Q(J)    + BR*RDZC(3,I);
  % MQ(J,J) = MQ(J,J) + MR;
end
