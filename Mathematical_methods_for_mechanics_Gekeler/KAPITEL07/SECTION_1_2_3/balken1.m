function [Z,ecode] = balken1(p,e,Lager,Lasten,Lastdichten,Parmeter);
% Berechnet Verschiebungen beim Biegebalken
% in spezieller Lage
Z = 0; ecode = 0;
N = length(p); 
 M = size(e,2) + 1
if N-M ~= 0
   disp(' Sorry, data do not match ! '),
   ecode = 1; return
end
N2 = 2*N;
KK = sparse(N2,N2); MM = sparse(N2,N2); BB = zeros(N2,1);
for I = 1:size(e,2)
   X = p(e(1:2,I)); H = e(3,I); B = e(4,I); RHO = e(5,I);
   [SE,ME,BE] = balkelement1(X,H,B,Parmeter);
   L       = e(1:2,I) - 1;
   M       = [1:2];
   J       = [2*L(1)+M, 2*L(2)+M];
   KK(J,J) = KK(J,J) + SE;
   MM(J,J) = MM(J,J) + ME;
   BB(J)   = BB(J) + Lastdichten(I)*BE;
end
AA = KK + MM;
% -- Lasten, rechte Seite abaendern -----------
BB(2*[1:N] - 1) = BB(2*[1:N]-1) + Lasten(1,:)';
BB(2*[1:N])     = BB(2*[1:N])   + Lasten(2,:)';
% -- Lager, Matrix und rechte Seite abaendern ! ------------------
J = find(Lager(1,:) ~= 0); % Verschiebung
J = 2*J - 1;
BB(J) = 0;
AA(J,:) = 0; AA(:,J) = 0;
for I = 1:length(J), AA(J(I),J(I))  = 1; end

J = find(Lager(2,:) ~= 0); % Momente
J = 2*J;
BB(J) = 0;
AA(J,:) = 0; AA(:,J) = 0;
for I = 1:length(J), AA(J(I),J(I))  = 1; end
% -- Loese LGS --------------------
X  = AA\BB;
Z  = reshape(X,2,N);
