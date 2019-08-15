function Y = flexiplex2(PROBLEMA,PROBLEMB,X,PHI,Parmeter1,Parmeter2)
% Parmeter1 for flexiplex
% Parmeter2 for fminsearch
ALFA = Parmeter1(1); BETA = Parmeter1(2); GAMA = Parmeter1(3); 
STEP = Parmeter1(4); GRAFIK = Parmeter1(5); 
MAXITER1 = Parmeter1(6); TOL = Parmeter1(7);
NX = length(X); PFAD = X; PHIOLD = PHI;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
STEP1  = STEP*(sqrt(NX + 1) + NX - 1)/(NX*sqrt(2));
STEP2  = STEP*(sqrt(NX + 1) - 1)/(NX*sqrt(2));
ETA = (STEP1 - (NX-1)*STEP2)/(NX+1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for KK = 1:8 %Step reduction
   V = start(X,STEP);
   if GRAFIK == 1
      plot([V(1,1:3),V(1,1)],[V(2,1:3),V(2,1)],'k'), hold on
      grid on
   end
   FV = zeros(1,NX+1); TV = FV;
   for I = 1:NX+1
      FV(I) = feval(PROBLEMA,V(:,I),1);
      TV(I) = feval(PROBLEMB,V(:,I));
   end
   [FV,J] = sort(FV); V = V(:,J); 
   % -- Main program --------------
   K = 0;  DONE = 0; 
   while ~DONE
      K = K+1;
      T = feval(PROBLEMB,V(:,1));
      if T > PHI
         X = fminsearcha(PROBLEMB,X,PHI,Parmeter2);
        if GRAFIK == 1; plot(X(1),X(2),'ro'); end
         V = start(X,STEP); %%% wichtig
         for I = 1:NX+1
            FV(I) = feval(PROBLEMA,V(:,I),1);
            TV(I) = feval(PROBLEMB,V(:,I));
         end
         [FV,J] = sort(FV); V = V(:,J); TV = TV(J);
         if TV(1) > PHI
            disp(' Bound violated ')
         end
      else
         PHI = max(T,PHI/2); % better
     %    PHI = T; % almost the same results  % good
         if GRAFIK == 1; plot(X(1),X(2),'bo'); end
      end
      % Reflection ----------------
      CENTER = sum(V(:,1:NX),2)/NX;
      XR = CENTER + ALFA*(CENTER - V(:,end));
      TR = feval(PROBLEMB,XR);
      if TR > PHI
         XR = fminsearcha(PROBLEMB,XR,PHI,Parmeter2);
      end
      FVR = feval(PROBLEMA,XR,1);
      if FVR < FV(1)
         X4 = XR + GAMA*(XR - CENTER);
         T4 = feval(PROBLEMB,X4);
         if T4 >= PHI
            X4 = fminsearcha(PROBLEMB,X4,PHI,Parmeter2);
         end
         FV4 = feval(PROBLEMA,X4,1);
         if FV4 <= FV(1)
            disp(' Expansion ')
            V(:,end) = X4; FV(end) = FV4;
         else
            disp(' Reflection ')
            V(:,end) = XR; FV(end) = FVR;
         end
         if GRAFIK == 1
            plot([V(1,:),V(1,1)],[V(2,:),V(2,1)],'g'), hold on
         end
      else
         if FVR < FV(end-1)
            disp(' second largest value ')
            V(:,end) = XR; FV(end) = FVR;
         else
            if FVR < FV(:,end)
               V(:,end) = XR;  FV(end) = FVR;
            end
            X5 = BETA*V(:,end) + (1 - BETA)*CENTER;
            disp(' Contraction ')
            T5 = feval(PROBLEMB,X5); F5 = feval(PROBLEMA,X5,1);
            if T5 > PHI
               X5 = fminsearcha(PROBLEMB,X5,PHI,Parmeter2);
            end   
            FV5 = feval(PROBLEMA,X5,1);
            if FV(end) > FV5
               V(:,end) = X5; FV(end) = FV5;
            else
               AUX = V(:,1);
               for I = 1:NX+1
                  V(:,I) = 0.5*(V(:,I) + AUX);
                  T(I) = feval(PROBLEMB,V(:,I));
                  if T(I) >= PHI
                     X6 = fminsearch(PROBLEMB,V(:,I));
                     FV(I) = feval(PROBLEMA,X6,1);
                  else   
                     FV(I) = feval(PROBLEMA,V(:,I),1);
                  end
                  disp(' Shrink ')
               end
            end
         end
         if GRAFIK == 1
      %     plot([V(1,:),V(1,1)],[V(2,:),V(2,1)],'r','linewidth',1), hold on
         end
      end
      [FV,J] = sort(FV); V = V(:,J); 
      % alternative possibility 
      X = V(:,1); %or
     % X = fminsearcha(PROBLEMB,V(:,1),PHI,Parmeter2);
      [PHI,DIFER] = tolcrit(V,PHI); 
      PFAD = [PFAD,X];
      if GRAFIK == 1
         plot(PFAD(1,:),PFAD(2,:),'k','linewidth',1), hold on
      end
      ITEROUT_ITER_DIFER_PHI = [KK,K,DIFER,PHI]
      %pause(0.5)
      DONE = (K == MAXITER1) | (DIFER < TOL);
      %%%% Possibly better results with %%%%
    %  if PHI == PHIOLD, PHI = PHI/2; end   
    %  PHIOLD = PHI; 
   end
   disp(' ----------- ')
   STEP = 0.05*DIFER; 
 %  if PHI == PHIOLD, PHI = PHI/2; end
 %  PHIOLD = PHI;
  pause 
end
Y = X; 
KRIT = feval(PROBLEMB,X)
if GRAFIK == 1, plot(X(1),X(2),'k*','markersize',12); end
%NAEHERUNG_OPT = [X,X_OPT]
