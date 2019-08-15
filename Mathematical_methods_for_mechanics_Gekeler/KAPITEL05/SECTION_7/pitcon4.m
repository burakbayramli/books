function [Y,errorcode] = pitcon4(F,WEG,TANG,J,H,Parmeter)
% NEWTON-methof for PITCON
errorcode = 0;
% -- Parameter ------------
THETA  = 1.05; MAXIT = 20; TOL = 1E-4;
% ---------------------------
[N1,M] = size(WEG);
V      = WEG(:,M) + H*TANG(:,M);
GRAD   = feval(F,V,2,Parmeter);
E      = zeros(1,N1); E(J) = 1;
GRAD   = [GRAD;E];
KOND = cond(GRAD);
RS     = feval(F,V,1,Parmeter);
RS     = [RS;0];
DY     = -GRAD\RS; Y = V + DY;
RS1    = feval(F,Y,1,Parmeter);
if max(abs(RS1)) >= 2*max(abs(RS)), errorcode = 1;
else YY = [V, Y]; end;
U      = V;
done   = errorcode;
iter   = 0;
while ~done
   iter  = iter + 1;
   Y     = U;
   E     = zeros(1,N1); E(J) = 1;
   RS    = feval(F,Y,1,Parmeter);
   RS    = [RS;Y(J) - V(J)];
   DY    = -GRAD\RS; Y = Y + DY;
   RSAUX = feval(F,Y,1,Parmeter);
   done1 = max(abs(RSAUX)) > THETA*max(abs(RS));
   done2 = max(abs(Y - YY(:,iter+1))) ...
           > THETA*max(abs(YY(:,iter+1) - YY(:,iter)));
   done3 = done1 | done2 | iter > MAXIT;
   done4 = max(abs(RSAUX)) < TOL;
   done  = done3 | done4;
  % DONE_1_2_3_4 = [done1,done2,done3,done4]
   if done3 errorcode = 1; else U = Y; YY = [YY,Y]; end
end
