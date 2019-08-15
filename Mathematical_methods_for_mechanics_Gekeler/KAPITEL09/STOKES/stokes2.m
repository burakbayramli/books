function [U,V,P,ecode] = stokes2(p,t,RDU,RDV,RDP,Parmeter);
% STOKES problem, Mini elements
% Declare one value for Pressure P
NU      = Parmeter(1);
F       = Parmeter(2);
EPSILON = Parmeter(3);
N  = size(p,2); M = size(t,2);
S  = sparse(N,N); MM = S; C = S; D = S;
B1 = ones(N,1); B2 = ones(N,1); B3 = zeros(N-1,1);
% Mini elements ------------------------
for I = 1:M
   K = t(1:3,I); XK = p(1,K); YK = p(2,K);
   [SE,ME,CE,DE,ecode] = mini(XK,YK);
   S(K,K)  = S(K,K) + SE;
   C(K,K)  = C(K,K) + CE;
   D(K,K)  = D(K,K) + DE;
   MM(K,K) = MM(K,K) + ME;
   if ecode == 1, disp(' wrong orientation '), return, end
end
% ATTENTION: Matrix [C; D] has not maximum rank ---
% --------------------------------------
B1 = F*B1; B2 = F*B2; S = NU*S;
B3 = zeros(N,1);
NULL = zeros(N,N);
A = [ S   , NULL, -C;
      NULL, S   , -D;
      C.' ,  D.', EPSILON*MM];
B = [B1; B2; B3];
% DIRICHLET boundary conditions for U -----
if ~isempty(RDU)
   J      = RDU(1,:);
   B      = B - A(:,J)*RDU(2,:)';
   B(J)   = RDU(2,:)';
   A(J,:) = 0; A(:,J) = 0;
   for I = 1:size(RDU,2), J = RDU(1,I); A(J,J)  = 1; end
end
% DIRICHLET boundary conditions for V -----
if ~isempty(RDV)
   J      = RDV(1,:) + N;
   B      = B - A(:,J)*RDV(2,:)';
   B(J)   = RDV(2,:)';
   A(J,:) = 0; A(:,J) = 0;
   for I = 1:size(RDV,2), J = RDV(1,I)+N; A(J,J)  = 1; end
end
% -- Condition for pressure -------------------
J      = RDP(1) + 2*N;
B      = B - A(:,J)*RDP(2);
B(J)   = RDP(2);
A(J,:) = 0; A(:,J) = 0; A(J,J)  = 1;

A = sparse(A);
% LGS ----------------------------------
disp(' Solving LGS ');
DD = scale(A);
A = DD*A; B = DD*B;
Z = A\B;
% P(N) = 0 setzen ! ---------------
U = Z(1:N); V = Z(N+1:2*N); P = Z(2*N+1:3*N);

%Q = [C;D];
%R = qr(Q);
%Z = diag(R)'

