function [MZP,Y,m] = adapt01(X0,GG,TT,Maxwert,Parmeter3)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Adaption of shooting points
n    = size(X0,1);
MZP  = 0; ANF  = X0(:,1); Flag = 1; I = 1;
while MZP(length(MZP)) < 1
   options = ...
    odeset('reltol',1.0E-3,'abstol',1.0E-5,'outputfcn',@mybound);
   [TA,YA] = ode23(GG,[MZP(I), 1],ANF,options,Flag,Parmeter3);
   TA = TA';        YA = YA';
   TL = length(TA); XA = zeros(n,TL);
   for K = 1:n
     XA(K,:) = interp1(TT,X0(K,:),TA);
   end
   DIFF = YA - XA;
   DIFFNORM = zeros(1,length(DIFF));
   for K = 1:length(DIFF)
      DIFFNORM(K) = norm(DIFF(:,K),inf);
   end
   AUX  = max(find(DIFFNORM <= Maxwert));
   MZP  = [MZP, TA(AUX)];
   ANF  = zeros(n,1);
   for K = 1:n
      ANF(K) = interp1(TT,X0(K,:),TA(AUX));
   end
   I = I+1;
end
mn = length(MZP); XNEU = zeros(n,mn);
for I = 1:n
   XNEU(I,:) = interp1(TT,X0(I,:),MZP);
end
Y = XNEU; m = mn-1;
