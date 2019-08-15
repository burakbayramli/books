function test02
% Testen quadratische Dreieckselemente
% partielle Verfeinerung

clear, clc, format short, format compact
tt = 0.1;
% ------------------------------
[p,e,t] = bsp001b(1);
clf, hold on
for I = 1:size(e,2)
    A = [p(1,e(1,I));p(1,e(2,I))];
    B = [p(2,e(1,I));p(2,e(2,I))];
    plot(A,B,'r'), hold on
end
axis equal, axis manual, grid on
X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
if ~isempty(t)
   trimesh(t',X,Y,Z), hold on
end
%pause
%
REFINE = 1;
for I = 1:REFINE
   disp(' Refinemesh ')
   it = [1,2,4,6,7];
   [p,e,t] = mesh01_t([],p,e,t,it);
      %p       = mesh10(p,e,t,5); % Jigglemesh
      %t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
      %t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen

   [p1,e,t1]  = mesh06_t(p,e,t);
   clf, hold on
   for I = 1:size(e,2)
       A = [p(1,e(1,I));p(1,e(2,I))];
       B = [p(2,e(1,I));p(2,e(2,I))];
       plot(A,B,'r'), hold on
   end
   axis equal, grid on
   X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
   trimesh(t',X,Y,Z,'edgecolor','r'), hold on
   pause(tt)
end
% -- Testen Zwischenpunkte fuer Dreiecke
if ~isempty(t)
   for I = 1:size(t,2)
      AAA = t(:,I); AAA = [AAA;AAA(1)];
      fill(p(1,AAA),p(2,AAA),'y'), hold on
      AA = t1(:,I) - size(p,2); %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      for K = 1:3
         plot(p1(1,AA(K)),p1(2,AA(K)),'r.'), hold on
         %pause
      end
      plot(p1(1,AA),p1(2,AA),'w.'), hold on
      %pause
   end
end
% -- Testen Zwischenpunkte fuer Randstuecke
p3 = [p,p1];
for I = 1:size(e,2)
   AA = e(1:2,I);
   plot(p(1,AA),p(2,AA),'k','linewidth',2), hold on
   BB = e(6,I);
      plot(p3(1,BB),p3(2,BB),'r*'), hold on
      pause(tt)
   end
end
