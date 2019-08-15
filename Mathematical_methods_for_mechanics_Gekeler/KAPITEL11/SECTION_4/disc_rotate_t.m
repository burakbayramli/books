function [M1N,M2N,XN,YN,X1N,Y1N] ...
         = disc_rotate_t(M1,M2,X,Y,X1,Y1,PP,Parmeter)
% E.GEKELER, RELEASE 12.3.04

TOL = 1E-3;

NN1     = Parmeter(1); NN2 = Parmeter(2); ITER = Parmeter(3);
MONITOR = Parmeter(4); A_AUF_B = Parmeter(5);
N1      = size(X1,2); N2 = size(Y1,2);
COS1    = zeros(1,NN2); J1  = zeros(1,NN1); Z1  = zeros(2,NN2);
COS2    = zeros(1,NN1); J2  = zeros(1,NN2); Z2  = zeros(2,NN1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% - MOEGLICHE NEUE DREHECKE IN SCHEIBE B BERECHNEN -
CS1 = NaN; CS2 = NaN;
%if ITER >= 5, MONITOR = 1; end
if MONITOR == 1
   clf
   axis([X1(1,1)-2,X1(1,1)+2,X1(2,1)-2,X1(2,1)+2])

   axis equal, axis manual, grid on, hold on
   plot(X1(1,:),X1(2,:),'r'), hold on
   plot(X1(1,:),X1(2,:),'.r'), hold on
   plot(Y1(1,[N2-2:N2,1:NN2]),Y1(2,[N2-2:N2,1:NN2]),'b'), hold on
   plot(Y1(1,1:NN2),Y1(2,1:NN2),'.b'), hold on
end
for I = 1:NN1
   AA = 1;
   DIFF1 = X1(:,I+1) - X1(:,1); DIST1 = norm(DIFF1);
   DIFF2 = Y1(:,1:NN2) - Y1(:,1)*ones(1,NN2);
   DIST2 = sqrt(DIFF2(1,:).^2 + DIFF2(2,:).^2);
   MAXP  = find(DIST1 > DIST2);
   MINP  = find(DIST1 < DIST2);
   if isempty(MAXP) | isempty(MINP), J2(I) = 1;
   else
      AUX1  = min(find(DIST2(MAXP) == max(DIST2(MAXP))));
      J2(I) = MAXP(AUX1);
   end
   A       = Y1(:,J2(I)); B = Y1(:,J2(I)+1);
   [Z2(:,I),errorcode] = schnitt(X1(:,1),DIST1,A,B,ITER,I,MONITOR,AA);
   if errorcode == 0
      DIFF3   = Z2(:,I) - X1(:,1);
      if norm(DIFF3) ~= 0 & DIST1 ~= 0
         COS2(I) = DIFF1'*DIFF3/(DIST1*norm(DIFF3));
      end
   end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% - MOEGLICHE NEUE DREHECKE IN SCHEIBE A BERECHNEN -
for I = 1:NN2
   AA = 2;
   DIFF2 = Y1(:,I+1) - Y1(:,1); DIST2 = norm(DIFF2);
   DIFF1 = X1(:,1:NN1) - X1(:,1)*ones(1,NN1);
   DIST1 = sqrt(DIFF1(1,:).^2 + DIFF1(2,:).^2);
   MAXP  = find(DIST2 > DIST1);
   MINP  = find(DIST2 < DIST1);
   if isempty(MAXP) | isempty(MINP), J1(I) = 1;
   else
      AUX1    = min(find(DIST1(MAXP) == max(DIST1(MAXP))));
      J1(I)   = MAXP(AUX1);
   end
   A       = X1(:,J1(I)); B = X1(:,J1(I)+1);
   [Z1(:,I),errorcode] = schnitt(Y1(:,1),DIST2,A,B,ITER,I,MONITOR,AA);
   if errorcode == 0
      DIFF3   = Z1(:,I) - Y1(:,1);
      if norm(DIFF3) ~= 0 & DIST2 ~= 0
         COS1(I) = DIFF2'*DIFF3/(DIST2*norm(DIFF3));
      end
   end
end
DREHPKT = X1(:,1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- COS DES DREHWINKELS BERECHNEN -----------------
%if MONITOR == 1, ACOS1 = COS1', ACOS2 = COS2', end

if max([COS1,COS2]) <= 0
   CS = 0; INDEX1 = 0; INDEX2 = 0;
   if MONITOR == 1
      ITER
      disp('Kein Einfuegen')
      CS
   end
else
   J = min(find(COS1 == max(COS1)));
   K = min(find(COS2 == max(COS2)));
   if COS2(K) >= COS1(J)
      CS  = COS2(K); INDEX1 = K; INDEX2 = J2(K);
      if norm(Z2(:,K)-Y1(:,INDEX2)) < TOL
         Y1 = [Y1(:,INDEX2:size(Y1,2)),Y1(:,1:INDEX2-1)];
      elseif norm(Z2(:,K)-Y1(:,INDEX2+1)) < TOL
         Y1 = [Y1(:,(INDEX2+1):size(Y1,2)),Y1(:,1:INDEX2)];
      else
         Y1 = [Z2(:,K),Y1(:,(INDEX2+1):size(Y1,2)),Y1(:,1:INDEX2)];
      end
      X1 = [X1(:,(INDEX1+1):size(X1,2)),X1(:,1:INDEX1)]; %OK__OK
      if MONITOR == 1,
         disp('Einfuegen in Scheibe B'),
         plot(Z2(1,:),Z2(2,:),'og'), hold on
         ITER_INDEX1 = [ITER,INDEX1]
         X1
      end
   else
      CS = COS1(J); INDEX1 = J1(J); INDEX2 = J;
      if norm(Z1(:,J)-X1(:,INDEX1)) < TOL
         X1 = [X1(:,INDEX1:size(X1,2)),X1(:,1:INDEX1-1)];
         if MONITOR == 1, disp('FALL = 1'); end
      elseif norm(Z1(:,J)-X1(:,INDEX1+1)) < TOL
         X1 = [X1(:,(INDEX1+1):size(X1,2)),X1(:,1:INDEX1)];
         if MONITOR == 1, disp('FALL = 2'); end
      else
         X1 = [Z1(:,J),X1(:,(INDEX1+1):size(X1,2)),X1(:,1:INDEX1)];
         if MONITOR == 1, disp('FALL = 3'), end
      end
      Y1 = [Y1(:,(INDEX2+1):size(Y1,2)),Y1(:,1:INDEX2)]; %OK
      if MONITOR == 1
         disp('Einfuegen in Scheibe A'),
         plot(Z1(1,:),Z1(2,:),'oc'), hold on
         ITER_INDEX2 = [ITER,INDEX2]
         X1
      end
   end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% -- DREHEN VON SCHEIBE A ------------------------
N1A   = size(X1,2); N2A = size(Y1,2);
if A_AUF_B == 1, SS = sqrt(1 - CS*CS); end  % A auf B
if A_AUF_B == 0; SS = -sqrt(1-CS*CS); end   % A in B
DREH = [CS, -SS; SS, CS];
M1N  = PP + DREH*(M1 - PP);
X1N  = PP*ones(1,N1A) + DREH*(X1-PP*ones(1,N1A));
XN   = PP*ones(1,size(X,2)) + DREH*(X-PP*ones(1,size(X,2)));
Y1N  = Y1; YN = Y; M2N = M2;
M1N  = real(M1N); X1N = real(X1N); XN   = real(XN);
if MONITOR == 1
   plot(X1N(1,:),X1N(2,:),'g'), hold on
   plot(X1N(1,:),X1N(2,:),'.g'), hold on

   plot(XN(1,:),XN(2,:),'k--'), hold on
   pause
end

FFF = 0;
if FFF == 1
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   % -- ZURUECKDREHEN VON SCHEIBE B -------------
   ss  = M1A(1)/sqrt(M1A(1)^2 + (M1A(2) - M2(2))^2);
   cs  = sqrt(1 - ss*ss); DREH2 = [cs, -ss; ss, cs];
   X1B = M2*ones(1,N1A) + DREH2*(X1A  - M2*ones(1,N1A));
   M1B = M2 + DREH2*(M1A - M2);
   Y1B = M2*ones(1,N2A) + DREH2*(Y1 - M2*ones(1,N2A));
   YN  = M2*ones(1,N2A) + DREH2*(Y - M2*ones(1,N2A));
   if INDX1 ~= 0 & INDX2 ~= 0
      X1B = [X1B(:,INDX1+1:N1A),X1B(:,1:INDX1)];
      Y1B = [Y1B(:,INDX2+1:N2A),Y1B(:,1:INDX2)];
   end
   X1N = X1B; Y1N = Y1B; M1N = M1B; M2N = M2;
end
if MONITOR == 2
   KK = NaN;
   NA = size(X1,2);
   DIFFC = X1(:,2:NA) - X1(:,1:NA-1);
   DISTC = sqrt(DIFFC(1,:).^2 + DIFFC(2,:).^2);
   ITER
   LL = find(DISTC == 0);
   if ~isempty(LL)
      KK = LL;
   end
   ITER_KK = [ITER,KK]
end
