function [p1,p2,e1,t1,q1] = mesh06_tq(p,e,t,q);
% Eckart Gekeler, Universitaet Stuttgart, Release 20.01.06
% Berechnung der Zwischenpunkte bei Dreiecken und Vierecken
% p1      : Zwischenpunkte der Dreiecke
% p2      : Zwischenpunkte der Vierecke
% t1(:,I) : Nrn der Zwischenpunkte in t(:,I);
% q1(:,I) : Nrn der Zwischenpunkte in q(:,I);
% e(6,I)  : Nr. des Zwischenpunktes in e(:,I);
% e WIRD DURCH EINE ZEILE ERGAENZT!
M = size(p,2); 
p1 = []; p2 = []; t1 = []; q1 = [];
e1 = [e;zeros(1,size(e,2))];
% -- Dreiecke --------------------
if ~isempty(t), MA = 0;
   for I = 1:size(t,2)
      M1 = MA+1; M2 = MA+2; M3 = MA+3;
      t1 = [t1,[M1; M2; M3]];
      J   = t(1:3,I);
      ZWP = [(p(:,J(1))+p(:,J(2)))/2, ...
            (p(:,J(2))+p(:,J(3)))/2, ...
            (p(:,J(3))+p(:,J(1)))/2];
      p1  = [p1,ZWP];
      MA  = MA + 3;
   end
   % -- doppelte Zwischenknoten beseitigen ----
   [M1,N1] = size(t1);
   [p1,I,J] = unique(p1','rows');
   p1 = p1'; C = t1(:); D = J(C);
   t1 = reshape(D,M1,N1);
   t1 = t1 + M; 
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   % Zwischenpunkte fuer Randstuecke
   for I = 1:size(e,2)
      ZWP = (p(:,e(1,I)) + p(:,e(2,I)))/2;
      DIFF = p1 - ZWP*ones(1,size(p1,2));
      NORMDIFF2 = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
      K = min(find(NORMDIFF2 < 100*eps));
      if ~isempty(K), e1(6,I) = M + K; end 
   end
end
% --- Vierecke -------------------------
if ~isempty(q),
   N = size(p1,2); q1 = []; MA = 0;
   for I = 1:size(q,2)
      M1 = MA+1; M2 = MA+2; M3 = MA+3; M4 = MA+4;
      q1 = [q1,[M1; M2; M3; M4]];
      J   = q(1:4,I);
      ZWP =[(p(:,J(1)) + p(:,J(2)))/2, ...
            (p(:,J(2)) + p(:,J(3)))/2, ...
            (p(:,J(3)) + p(:,J(4)))/2, ...
            (p(:,J(4)) + p(:,J(1)))/2];
      p2  = [p2,ZWP];
      MA  = MA + 4;
   end
   % -- doppelte Zwischenknoten beseitigen ----
   [M1,N1] = size(q1);
   [p2,I,J] = unique(p2','rows');
   p2 = p2'; C = q1(:); D = J(C);
   q1 = reshape(D,M1,N1);
   % -- Zwischenpunkte fuer Randstuecke --------
   for I = 1:size(e,2)
      ZWP = (p(:,e(1,I)) + p(:,e(2,I)))/2;
      DIFF = p2 - ZWP*ones(1,size(p2,2));
      NORMDIFF2 = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
      K = min(find(NORMDIFF2 < 100*eps));
      if ~isempty(K), e1(6,I) = M + N + K; end 
   end
   % -- gemeinsame Kanten beruecksichtigen -----
   M = size(p,2); N = size(p1,2);
   q1 = M + N + q1;   
   p3 = [p,p1,p2]; 
   [p3,q1] = mesh04(p3,q1);  % wg. Ordnung
   p2 = p3(:,M+N+1:size(p3,2));
   % -- Zwischenpunkte fuer Randstuecke --------
   for I = 1:size(e,2)
      ZWP = (p(:,e(1,I)) + p(:,e(2,I)))/2;
      DIFF = p2 - ZWP*ones(1,size(p2,2));
      NORMDIFF2 = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
      K = min(find(NORMDIFF2 < 100*eps));
      if ~isempty(K), e1(6,I) = M + N + K; end 
   end
end 
% -- Probe auf Eindeutigkeit
%   AUX = [p,p1,p2];
%[DD,I,J] = unique(AUX','rows');
%II = sort(I');
%JJ = sort(J');
%DIFF = norm(II - JJ)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
