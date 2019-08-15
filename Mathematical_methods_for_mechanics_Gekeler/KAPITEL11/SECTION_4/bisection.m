function [YN,PSI,ERRORCODE] = bisection(X,Y,Parmeter);
% E.GEKELER, RELEASE 21.3.04
% Scheibe A laeuft in Scheibe B
TOL = Parmeter(1); A_AUF_B = Parmeter(2);
OPTION = Parmeter(3); DPHI = Parmeter(4);
M1 = Parmeter(5:6); M2 = Parmeter(7:8); ITER = Parmeter(9);
YN = Y; ITERB = 1; PSI = 0; ERRORCODE = 0;
[AINB,AONB] = inpolygon(X(1,:),X(2,:),Y(1,:),Y(2,:));
J = find(AONB == 1); if ~isempty(J), AINB(J) = 0, end
K = find(AINB == 1);
DONE = isempty(K) & ~isempty(J);
while ~DONE
   PHI1 = 0; PHI3 = DPHI*ITERB;
   done = 0; ITERC = 1;
   while ~done
      PSI = (PHI1 + PHI3)/2;
      cs   = cos(PSI); ss = sin(PSI);
      DREH = [cs, -ss; ss, cs];
      YN  = DREH*Y;
      if A_AUF_B == 1
         [AINB,AONB] = inpolygon(X(1,:),X(2,:),YN(1,:),YN(2,:));
      else
         [AINB,AONB] = inpolygon(YN(1,:),YN(2,:),X(1,:),X(2,:));
      end
      J = find(AONB == 1);
      if ~isempty(J), AINB(J) = 0; end
      L = find(AINB == 1);
      if isempty(L) PHI3 = PSI; else PHI1 = PSI; end;
      MONITOR = 0;
      %if ITER >= 12, MONITOR = 1; end
      if MONITOR == 1
         if isempty(L) A_IN_B = NaN; else A_IN_B = L; end
         ITER_ITERB_ITERC_PSI = [ITER,ITERB,ITERC,PSI], %A_IN_B
         save datena M1 M2 X YN, bild01a, pause, ITERC = ITERC +1;
      end
      done = abs(PHI1 - PHI3) < TOL ;
   end,
   %disp('---------------------')
   Y = YN;
   if A_AUF_B == 1
      [AINB,AONB] = inpolygon(X(1,:),X(2,:),Y(1,:),Y(2,:));
   else
      [AINB,AONB] = inpolygon(Y(1,:),Y(2,:),X(1,:),X(2,:));
   end
   J = find(AONB == 1); if ~isempty(J), AINB(J) = 0, end
   K = find(AINB == 1);
   ITERB = ITERB + 1;
   if ITERB == 5, ERRORCODE = 1, disp('Scheiben verkantet!'), end
   LL = 2; % WAEHLEN!!!!!CASE 2: LL = 1;
   DONE = length(K) <= LL | ITERB >= 5;
end
YN = Y;