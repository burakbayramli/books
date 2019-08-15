function r = maratos(F,x,d,y,Parmeter);
% Zwei Versionen von Maratos
g0     = feval(F,x+d,3,Parmeter);
h0     = feval(F,x+d,5,Parmeter);
gradg  = feval(F,x,4,Parmeter);
gradh  = feval(F,x,6,Parmeter);
if ~isempty(g0)
   N = gradh.'; J = find(y > 0);
   if ~isempty(J)
      N = [gradh.',gradg(J,:).']; h0 = [h0;g0(J)];
   end   
else
   N = gradh.';
end      
[Q_T,R] = qr(N);
[MR,NR] = size(R);
R_T     = R(1:NR,:).';
AUX     = R_T\h0;
EE      = [eye(NR);zeros(MR-NR,NR)];
r       = Q_T*EE*AUX;
% 2. Version ---------------
MM = N.'*N;
CC = MM\h0;
rr = N*CC;
diff = norm(r-rr)
%pause
