function [un,vn,wn,u_aux] = hem4_kern_s(name,t,tau,u,v,w,parmtr2,parmtr3)
% halb-explizites RKV nach Brasey fuer mechanische Systeme
% r = 5-stufiges Verfahren
% wie HEM4_KERN.M aber mit Schleifen
% -- RKV Daten -----------------
r    = 5;
sqr6 = sqrt(6);
A = [0,                 0,             0,                0;
     3/10,              0,             0,                0;
     (1+sqr6)/30,      (11-4*sqr6)/30, 0,                0;
     (-79-31*sqr6)/150,(-1-4*sqr6)/30,(24 + 11*sqr6)/25, 0;
     (14+5*sqr6)/6,    (-8+7*sqr6)/6, (-9-7*sqr6)/4,(9-sqr6)/4];
b = [0, 0,   (16-sqr6)/36,(16+sqr6)/36, 1/9];
c = [0, 3/10,(4-sqr6)/10, (4+sqr6)/10,  1];
A = [[A, zeros(r,2)];[b, 0]];
A1 = A; A1(2,1) = 0; A1(3,2) = 0; A1(4,3) = 0;
A1(5,4) = 0; A1(6,5) = 0;
% ----------------------------------
ecode = 0;
un = u; vn = v; wn = w;
dimu   = length(u);
dimw   = length(w);
FF     = [name];
U      = zeros(dimu,r+1); V = U; DV = U; W = zeros(dimw,r);
U(:,1) = u; V(:,1) = v;
TT     = [t + tau*c,t + tau];
for I = 1:5
   U(:,I+1) = U(:,1) + tau*V*A(I+1,:)';
   F  = feval(FF,1,TT(I),U(:,I),V(:,I),W(:,I),parmtr3);
   H  = feval(FF,4,TT(I),U(:,I),V(:,I),W(:,I),parmtr3);
   G1 = feval(FF,3,TT(I),U(:,I),V(:,I),W(:,I),parmtr3);
   M  = feval(FF,5,TT(I),U(:,I),V(:,I),W(:,I),parmtr3);
   G2 = feval(FF,3,TT(I+1),U(:,I+1),V(:,I),W(:,I),parmtr3);
   AA = [M, G1';G2, zeros(dimw,dimw)];
   %CN = cond(A);
   AUXDV = DV*A1(I+1,:)';
   R       = - (G2*(V(:,1) + tau*AUXDV) + H)/(tau*A(I+1,I));
   RS      = [F;R];
   XX      = AA\RS;
   DV(:,I) = XX(1:dimu);
   W(:,I)  = XX(dimu+1:dimu+dimw);
   V(:,I+1) = V(:,1) + tau*DV*A(I+1,:)';
end
u_aux = U(:,5)
un    = U(:,6)
vn    = V(:,6);
wn    = W(:,5);
