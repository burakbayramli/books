function [RDKN,RDEL,OFFSET,MAXL] = mesh40(KNOTEN,e,segnr,ELEMENTE);
% Eckart Gekeler, Universitaet Stuttgart, Release 10.4.05
% berechnet zu jedem Randpunkt die angrenzenden Elemente
% und die Nachbarknoten

RDKN = 0; RDEL = 0; MAXG = 0; M = size(ELEMENTE,2);
E = [];
for I = 1:length(segnr)
   J = find(e(5,:) == segnr(I)); E = [E,e(1:2,J)];
end
R = E(1,:); RAND = E(1:2,:);
KK = zeros(15,length(R));
for I = 1:length(R)
   G = [];
   for J = 1:M
      if any(ELEMENTE(1:3,J) == R(I)), G = [G;J]; end
   end
   KK(:,I) = [G; zeros(15-length(G),1)];
   if length(G) > MAXG, MAXG = length(G); end
end
KK = KK(1:MAXG,:); RDEL = KK(:); RDEL = sort(RDEL);
for I = 1:length(RDEL)-1
       if RDEL(I) == RDEL(I+1); RDEL(I+1) = 0; end
end
J = find(RDEL ~= 0); RDEL = RDEL(J);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
RDKN = zeros(3*MAXG,length(R)); MAXRL = 0;
for I = 1:length(R)
    RL = [];
    for K = 1:MAXG
        if KK(K,I) ~= 0, RL = [RL;ELEMENTE(1:3,KK(K,I))]; end
    end
    RL = sort(RL);
    for I1 = 1:length(RL)-1
       if RL(I1) == RL(I1+1); RL(I1+1) = 0; end
    end
    J = find(RL ~= 0); RL = RL(J);
    for I2 = 1:length(RL)
       for J = 1:length(R)
          if any(RAND(1,J) == RL(I2)) | any(RAND(2,J) == RL(I2))
             RL(I2) = 0;
          end
       end
    end
    J = find(RL ~= 0); RL = RL(J);
    if length(RL) > MAXRL, MAXRL = length(RL); end
    RDKN(:,I) = [RL; zeros(3*MAXG - length(RL),1)];
end
RDKN = RDKN(1:MAXRL,:);
% -- Randknoten muessen geordnet werden -----
for I = 1:length(R)
   J = find(RDKN(:,I) ~= 0);
   AUX = RDKN(J,I);
   PDIFF = KNOTEN(:,AUX) - KNOTEN(:,R(I))*ones(1,length(AUX));
   [PHI,RR] = cart2pol(PDIFF(1,:),PDIFF(2,:));
   [PSI,K] = sort(PHI);
   AUX = AUX(K);
   RDKN(J,I) = AUX;
   if max(PHI) > pi/2 & min(PHI) < - pi/2
      RDKN(J,I) = flipud(AUX);
   end
   RDKN(:,I) = flipud(RDKN(:,I));
end
RDKNAUX = RDKN(:); J = find(RDKNAUX ~= 0);
RDKNAUX = RDKNAUX(J); OFFSET = [];
for I = 2:length(RDKNAUX)
   for K = 1:I-1
      if RDKNAUX(I) == RDKNAUX(K), RDKNAUX(I) = 0; end
   end
end
J = find(RDKNAUX ~= 0); OFFSET = RDKNAUX(J)';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Berechnung der max. Seitenlaenge der Randdreiecke
tt = ELEMENTE(1:3,RDEL);
tt = [tt;tt(1,:)];
MAXL = 0;
for I = 1:3
   X1 = KNOTEN(1,tt(I,:)); X2 = KNOTEN(1,tt(I+1,:));
   Y1 = KNOTEN(2,tt(I,:)); Y2 = KNOTEN(2,tt(I+1,:));
   LAENGEN = sqrt((X1 - X2).^2 + (Y1 - Y2).^2); 
   MAXL = max(MAXL,max(LAENGEN));
end

