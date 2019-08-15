function [un,vn,wn,u_aux] = hem4_kern(fcn,t,tau,u,v,w,parmtr2,parmtr3)
% halb-explizites RKV nach Brasey fuer mechanische Systeme
% r = 5-stufiges Verfahren
% -- RKV Daten -----------------
sqr6 = sqrt(6);
A = [0,                 0,             0,                0;
     3/10,              0,             0,                0;
     (1+sqr6)/30,      (11-4*sqr6)/30, 0,                0;
     (-79-31*sqr6)/150,(-1-4*sqr6)/30,(24 + 11*sqr6)/25, 0;
     (14+5*sqr6)/6,    (-8+7*sqr6)/6, (-9-7*sqr6)/4,(9-sqr6)/4];
b = [0, 0,   (16-sqr6)/36,(16+sqr6)/36, 1/9];
c = [0, 3/10,(4-sqr6)/10, (4+sqr6)/10,  1];
% ----------------------------------
dimu = length(u); dimw = length(w);
r    = 5; TT = [t + tau*c];
U      = zeros(dimu,r+1); V = U; DV = U; W = zeros(dimw,r);
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
   R  = - (G2*V(:,1) + H)/(tau*A(2,1));
   RS = [F;R]; XX = AA\RS;
   DV(:,1) = XX(1:dimu); W(:,2)  = XX(dimu+1:dimu+dimw);
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
   R  = - (G3*(V(:,1) + tau*A(3,1)*DV(:,1)) + H)/(tau*A(3,2));
   RS = [F;R]; XX = AA\RS;
   DV(:,2) = XX(1:dimu); W(:,3)  = XX(dimu+1:dimu+dimw);
   V(:,3) = V(:,1) + tau*A(3,1)*DV(:,1) + tau*A(3,2)*DV(:,2);
   % -- 4. Stufe  --------------------------
   U(:,4) = U(:,1) + tau*(A(4,1)*V(:,1) + A(4,2)*V(:,2) + A(4,3)*V(:,3));
   F  = feval(fcn,1,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
   H  = feval(fcn,4,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
   G3 = feval(fcn,3,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
   M  = feval(fcn,5,TT(3),U(:,3),V(:,3),W(:,3),parmtr3);
   G4 = feval(fcn,3,TT(4),U(:,4),V(:,3),W(:,3),parmtr3);
   AA = [M, G3';G4, zeros(dimw,dimw)];
   %CN = cond(A);
   R  = - (G4*(V(:,1) + tau*A(4,1)*DV(:,1) ...
             + tau*A(4,2)*DV(:,2))+ H)/(tau*A(4,3));
   RS = [F;R]; XX = AA\RS;
   DV(:,3) = XX(1:dimu); W(:,4) = XX(dimu+1:dimu+dimw);
   V(:,4) = V(:,1) + tau*(A(4,1)*DV(:,1) + A(4,2)*DV(:,2) + A(4,3)*DV(:,3));
   % -- 5. Stufe  --------------------------
   U(:,5) = U(:,1) + tau*(A(5,1)*V(:,1) + A(5,2)*V(:,2) ...
            + A(5,3)*V(:,3) + A(5,4)*V(:,4));
   F  = feval(fcn,1,TT(4),U(:,4),V(:,4),W(:,4),parmtr3);
   H  = feval(fcn,4,TT(4),U(:,4),V(:,4),W(:,4),parmtr3);
   G4 = feval(fcn,3,TT(4),U(:,4),V(:,4),W(:,4),parmtr3);
   M  = feval(fcn,5,TT(4),U(:,4),V(:,4),W(:,4),parmtr3);
   G5 = feval(fcn,3,TT(5),U(:,5),V(:,4),W(:,4),parmtr3);
   AA = [M, G4';G5, zeros(dimw,dimw)];
   %CN = cond(A);
   R  = - (G5*(V(:,1) + tau*A(5,1)*DV(:,1) + tau*A(5,2)*DV(:,2) ...
        +  tau*A(5,3)*DV(:,3)) + H)/(tau*A(5,4));
   RS = [F;R]; XX = AA\RS;
   DV(:,4) = XX(1:dimu); W(:,5) = XX(dimu+1:dimu+dimw);
   V(:,5) = V(:,1) + tau*(A(5,1)*DV(:,1) + A(5,2)*DV(:,2) ...
            + A(5,3)*DV(:,3) + A(5,4)*DV(:,4));
   % -- Vorwaertsschritt --------------------------
   U(:,6) = U(:,1) + tau*(b(1)*V(:,1) + b(2)*V(:,2) ...
            + b(3)*V(:,3) + b(4)*V(:,4) + b(5)*V(:,5));
   F  = feval(fcn,1,TT(5),U(:,5),V(:,5),W(:,5),parmtr3);
   H  = feval(fcn,4,TT(5),U(:,5),V(:,5),W(:,5),parmtr3);
   G5 = feval(fcn,3,TT(5),U(:,5),V(:,5),W(:,5),parmtr3);
   M  = feval(fcn,5,TT(5),U(:,5),V(:,5),W(:,5),parmtr3);
   G6 = feval(fcn,3,TT(5),U(:,6),V(:,5),W(:,5),parmtr3);
   AA = [M, G5';G6, zeros(dimw,dimw)];
   %CN = cond(A);
   R  = - (G6*(V(:,1) + tau*b(1)*DV(:,1) + tau*b(2)*DV(:,2) ...
        +  tau*b(3)*DV(:,3) + tau*b(4)*DV(:,4)) + H)/(tau*b(5));
   RS = [F;R]; XX = AA\RS;
   DV(:,5) = XX(1:dimu); W(:,6)  = XX(dimu+1:dimu+dimw);
   V(:,6) = V(:,1) + tau*(b(1)*DV(:,1) + b(2)*DV(:,2) ...
            + b(3)*DV(:,3) + b(4)*DV(:,4) + b(5)*DV(:,5));
% -------------------------------------------------------
u_aux = U(:,5);
un    = U(:,6);
vn    = V(:,6);
wn    = W(:,6);
