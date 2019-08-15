function [M1N,M2N,XN,YN,X1N,Y1N] ...
         = disc_rot_neu(M1,M2,X,Y,X1,Y1,PP,Parmeter)
% E.GEKELER, RELEASE 28.11.04
NN1 = Parmeter(1); NN2 = Parmeter(2); TOL = Parmeter(3);
A_AUF_B = Parmeter(4); OPTION = Parmeter(5); MINCOS = Parmeter(6);
NX = size(X,2); NY = size(Y,2); NX1 = size(X1,2); NY1 = size(Y1,2);
COS1 = zeros(1,NN2); J1  = zeros(1,NN1); Z1  = zeros(2,NN2);
COS2 = zeros(1,NN1); J2  = zeros(1,NN2); Z2  = zeros(2,NN1);
% -- MOEGLICHE NEUE DREHECKE IN SCHEIBE B BERECHNEN -----------------
for I = 1:NN1
   DIFF1 = X1(1:2,I+1) - X1(1:2,1); DIST1 = norm(DIFF1);
   DIFF2 = Y1(1:2,1:NN2) - Y1(1:2,1)*ones(1,NN2);
   DIST2 = sqrt(DIFF2(1,:).^2 + DIFF2(2,:).^2);
   MAXP  = find(DIST1 > DIST2);   MINP  = find(DIST1 < DIST2);
   if isempty(MAXP) | isempty(MINP), J2(I) = 1;
   else
      J2(I) = MAXP(min(find(DIST2(MAXP) == max(DIST2(MAXP)))));
   end
   A = Y1(1:2,J2(I)); B = Y1(1:2,J2(I)+1);
   [Z2(:,I),ecode] = schnitt(X1(1:2,1),DIST1,A,B);
   DIFF   = Z2(:,I) - X1(1:2,1);
   if ecode == 0 & norm(DIFF) ~= 0 & DIST1 ~= 0
      COS2(I) = DIFF1'*DIFF/(DIST1*norm(DIFF));
   end
end
% -- MOEGLICHE NEUE DREHECKE IN SCHEIBE A BERECHNEN -----------------
for I = 1:NN2
   DIFF2 = Y1(1:2,I+1) - Y1(1:2,1); DIST2 = norm(DIFF2);
   DIFF1 = X1(1:2,1:NN1) - X1(1:2,1)*ones(1,NN1);
   DIST1 = sqrt(DIFF1(1,:).^2 + DIFF1(2,:).^2);
   MAXP  = find(DIST2 > DIST1); MINP  = find(DIST2 < DIST1);
   if isempty(MAXP) | isempty(MINP), J1(I) = 1;
   else
      J1(I)    = MAXP(min(find(DIST1(MAXP) == max(DIST1(MAXP)))));
   end
   A = X1(1:2,J1(I)); B = X1(1:2,J1(I)+1);
   [Z1(:,I),ecode] = schnitt(Y1(1:2,1),DIST2,A,B);
   DIFF   = Z1(:,I) - Y1(1:2,1);
   if ecode == 0 & norm(DIFF) ~= 0 & DIST2 ~= 0
      COS1(I) = DIFF2'*DIFF/(DIST2*norm(DIFF));
   end
end
% -- CS := COS(DREHWINKEL) und DREHECKE BERECHNEN -----------------
J = min(find(COS1 == max(COS1)));
K = min(find(COS2 == max(COS2)));
if (COS2(K) >= COS1(J) & COS2(K) < MINCOS) | ...
   (COS1(J) >= COS2(K) & COS1(J) < MINCOS) | ...
   (max([COS1,COS2]) <= 0)
   CS = MINCOS; INDEX1 = 0; INDEX2 = 0;  % DIESEN FALL NICHT WEGLASSEN!
   X1A = X1; Y1A = Y1;
end
if (COS2(K) >= COS1(J)) & (COS2(K) >= MINCOS)
   CS  = COS2(K); INDEX1 = K; INDEX2 = J2(K);
   if norm(Z2(:,K)-Y1(1:2,INDEX2)) < TOL
      Y1A = [Y1(:,INDEX2:NY1),Y1(:,1:INDEX2-1)];
   elseif norm(Z2(:,K)-Y1(1:2,INDEX2+1)) < TOL
      Y1A = [Y1(:,(INDEX2+1):NY1),Y1(:,1:INDEX2)];
   else
      Y1A = [[Z2(:,K);2],Y1(:,(INDEX2+1):NY1),Y1(:,1:INDEX2)];
   end
   X1A = [X1(:,(INDEX1+1):NX1),X1(:,1:INDEX1)];
   X1 = X1A; Y1 = Y1A;
   J = find(X1(3,:) ~= 0); X1 = X1(:,J);
   J = find(X1(3,:) == 2); X1(3,J) = 0;
   J = find(Y1(3,:) ~= 0); Y1 = Y1(:,J);
   J = find(Y1(3,:) == 2); Y1(3,J) = 0;
end
if (COS1(J) > COS2(K)) & (COS1(J) >= MINCOS)
   CS = COS1(J); INDEX1 = J1(J); INDEX2 = J;
   if norm(Z1(:,J)-X1(1:2,INDEX1)) < TOL
      X1A = [X1(:,INDEX1:NX1),X1(:,1:INDEX1-1)];
   elseif norm(Z1(:,J)-X1(1:2,INDEX1+1)) < TOL
      X1A = [X1(:,(INDEX1+1):NX1),X1(:,1:INDEX1)];
   else
      X1A = [[Z1(:,J);2],X1(:,(INDEX1+1):NX1),X1(:,1:INDEX1)];
   end
   Y1A = [Y1(:,(INDEX2+1):NY1),Y1(:,1:INDEX2)];
   X1 = X1A; Y1 = Y1A;
   J = find(X1(3,:) ~= 0); X1 = X1(:,J);
   J = find(X1(3,:) == 2); X1(3,J) = 0;
   J = find(Y1(3,:) ~= 0); Y1 = Y1(:,J);
   J = find(Y1(3,:) == 2); Y1(3,J) = 0;
end
% -- DREHEN VON SCHEIBE A ------------------------
N1A   = size(X1,2); N2A = size(Y1,2);
X1N = zeros(3,N1A); X1N(3,:) = X1(3,:);
if A_AUF_B == 1, SS = sqrt(1 - CS*CS); end  % A auf B
if A_AUF_B == 0; SS = -sqrt(1-CS*CS); end   % A in B
DREH = [CS, -SS; SS, CS];
M1N  = PP + DREH*(M1 - PP);
M2N  = M2;
XN(1:2,:)   = PP*ones(1,NX) + DREH*(X(1:2,:)-PP*ones(1,NX));
YN   = Y;
X1N(1:2,:)  = PP*ones(1,N1A) + DREH*(X1(1:2,:)-PP*ones(1,N1A));
Y1N  = Y1;
M1N  = real(M1N); XN = real(XN); X1N = real(X1N);
% -- ZURUECKDREHEN VON BEIDEN SCHEIBEN -------------
if OPTION  == 1
   DIFF = M1N - M1;
   A = norm(DIFF); B = norm(M1N); C = norm(M1);
   cs = (B^2 + C^2 - A^2)/(2*B*C);
   ss  = sqrt(1 - cs*cs); DREH2 = [cs, ss; -ss, cs];
   M1N = M2             + DREH2*(M1N - M2);
   M2N = M2;
   XN(1:2,:)  = M2*ones(1,NX)  + DREH2*(XN(1:2,:) - M2*ones(1,NX));
   YN(1:2,:)  = M2*ones(1,NY)  + DREH2*(YN(1:2,:) - M2*ones(1,NY));
   X1N(1:2,:) = M2*ones(1,N1A) + DREH2*(X1N(1:2,:)  - M2*ones(1,N1A));
   Y1N(1:2,:) = M2*ones(1,N2A) + DREH2*(Y1N(1:2,:) - M2*ones(1,N2A));
   M1N  = real(M1N); XN = real(XN); YN   = real(YN);
   X1N = real(X1N); Y1N = real(Y1N);
end
