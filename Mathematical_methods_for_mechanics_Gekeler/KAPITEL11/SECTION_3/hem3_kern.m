function [un,vn,wn,u_aux] = ...
hem3_kern(fcn,t,tau,u,v,w,parmtr2,parmtr3)
% halb-explizites RKV nach Brasey fuer mechanische Systeme
% r = 3-stufiges Verfahren
% -- RKV Daten -----------------
A = [0, 0, 0; 1/3, 0, 0; -1, 2, 0];
b = [0, 3/4, 1/4];
c = [0, 1/3, 1];
% ----------------------------------
ecode = 0;
dimu = length(u); dimw = length(w);
r = 3; TT = [t + tau*c];
U = zeros(dimu,r+1); V = U; DV = U; W = zeros(dimw,r);
% -- 1. Stufe ---------------------------
U(:,1) = u; V(:,1) = v; W(:,1) = w;
% -- 2. Stufe  --------------------------
U(:,2) = U(:,1) + tau*A(2,1)*V(:,1);
F  = feval(fcn,1,TT(1),U(:,1),V(:,1),W(:,1),parmtr3);
H  = feval(fcn,4,TT(1),U(:,1),V(:,1),W(:,1),parmtr3);
G1 = feval(fcn,3,TT(1),U(:,1),V(:,1),W(:,1),parmtr3);
M  = feval(fcn,5,TT(1),U(:,1),V(:,1),W(:,1),parmtr3);
G2 = feval(fcn,3,TT(2),U(:,2),V(:,1),W(:,1),parmtr3);
AA = [M, G1';G2, zeros(dimw,dimw)];
%CN = cond(A);
R       = - (G2*V(:,1) + H)/(tau*A(2,1));
RS      = [F;R]; XX = AA\RS;
DV(:,1) = XX(1:dimu);
W(:,2)  = XX(dimu+1:dimu+dimw);
V(:,2) = V(:,1) + tau*A(2,1)*DV(:,1);
% -- 3. Stufe  --------------------------
U(:,3) = U(:,1) + tau*A(3,1)*V(:,1) + tau*A(3,2)*V(:,2);
F  = feval(fcn,1,TT(2),U(:,2),V(:,2),W(:,2),parmtr3);
H  = feval(fcn,4,TT(2),U(:,2),V(:,2),W(:,2),parmtr3);
G2 = feval(fcn,3,TT(2),U(:,2),V(:,2),W(:,2),parmtr3);
M  = feval(fcn,5,TT(2),U(:,2),V(:,2),W(:,2),parmtr3);
G3 = feval(fcn,3,TT(3),U(:,3),V(:,2),W(:,2),parmtr3);
AA = [M, G2';G3, zeros(dimw,dimw)];
%CN = cond(A);
R       = - (G3*(V(:,1) + tau*A(3,1)*DV(:,1)) + H)/(tau*A(3,2));
RS      = [F;R]; XX = AA\RS;
DV(:,2) = XX(1:dimu);
W(:,3)  = XX(dimu+1:dimu+dimw);
V(:,3)  = V(:,1) + tau*A(3,1)*DV(:,1) + tau*A(3,2)*DV(:,2);
% -- Vorwaertsschritt  --------------------------
U(:,4) = U(:,1) + tau*(b(1)*V(:,1) + b(2)*V(:,2) + b(3)*V(:,3));
F  = feval(fcn,1,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
H  = feval(fcn,4,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
G3 = feval(fcn,3,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
M  = feval(fcn,5,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
G4 = feval(fcn,3,TT(3),U(:,4),V(:,3),W(:,3),parmtr3);
AA = [M, G3';G4, zeros(dimw,dimw)];
%CN = cond(A);
R       = - (G4*(V(:,1) + tau*b(1)*DV(:,1)...
          + tau*b(2)*DV(:,2))+ H)/(tau*b(3));
RS      = [F;R]; XX = AA\RS;
DV(:,3) = XX(1:dimu); W(:,4)  = XX(dimu+1:dimu+dimw);
V(:,4)  = V(:,1) + tau*(b(1)*DV(:,1) + b(2)*DV(:,2) + b(3)*DV(:,3));
u_aux = U(:,3); un = U(:,4); vn = V(:,4); wn = W(:,4);
