function [X,ecode] = stabwerk1(p,e,LAGER,LASTEN);
% GEKELER: Math. Meth. Mech.
% berechnet Kraefte in ebenen Fachwerken

ecode  = 0;
M1 = size(p,2);
M2 = size(LAGER,2);
M3 = size(e,2);
N  = 2*M1;
if N ~= M2+M3
   disp(' falsche Systemdimension')
   X = NaN*ones(N,1); ecode = 1; return;
end
STB_X  = p(1,e(2,:)) - p(1,e(1,:));
STB_Y  = p(2,e(2,:)) - p(2,e(1,:));
INVLG  = ones(1,M3)./sqrt(STB_X.*STB_X + STB_Y.*STB_Y);
STAEBE = [STB_X;STB_Y]*diag(INVLG);
A      = sparse(N,M3);
for I = 1:M1
   J = find(e(1,:) == I);
   K = find(e(2,:) == I);
   A(2*I-1:2*I,J) =   STAEBE(:,J);
   A(2*I-1:2*I,K) = - STAEBE(:,K);
end
% LAGER --------------------------------------------
C = sparse(N,M2);
for I = 1:M2
    K = LAGER(1,I);
    C(2*K-1:2*K,I) = LAGER(2:3,I);
end
B = LASTEN(:);
% Loesung LGS --------------------------------------
A = [A, -C];
X = A\B;
