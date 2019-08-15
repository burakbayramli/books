function test01
% Testen quadratische Dreieckselemente
% und quadratische Parallelogrammelemente
clear, clc, format short, format compact

REFINE = 1;   % Anzahl Gitterverfeinerungen
tt = 0.01;
% ------------------------------
[p,e,t,q] = bsp001b(2);

for I = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t,q] = mesh01_tq([],p,e,t,q);
end
[p1,p2,e,t1,q1]  = mesh06_tq(p,e,t,q);
save daten1 p e t q  p1 p2 t1 q1
clf, hold on
for I = 1:size(e,2)
    A = [p(1,e(1,I));p(1,e(2,I))];
    B = [p(2,e(1,I));p(2,e(2,I))];
    plot(A,B,'r'), hold on
end
axis equal, axis manual, grid on
X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
trimesh(t',X,Y,Z), hold on
mesh36(p,q,'b'), hold on
% -- Testen Zwischenpunkte fuer Dreiecke
if ~isempty(t)
   for I = 1:size(t,2)
      AAA = t(:,I); AAA = [AAA;AAA(1)];
      fill(p(1,AAA),p(2,AAA),'y'), hold on
      AA = t1(:,I) - size(p,2); %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      for K = 1:3
         plot(p1(1,AA(K)),p1(2,AA(K)),'r.'), hold on
         %pause(tt)
      end
      plot(p1(1,AA),p1(2,AA),'w.'), hold on
      %pause(tt)
   end
end
% -- Testen Zwischenpunkte fuer Parallelogrammelemente
if ~isempty(q)
   p_aux = [p,p1,p2];
   for I = 1:size(q,2)
      AA = q(:,I); AA = [AA;AA(1)];
      fill(p(1,AA),p(2,AA),'g'), hold on
      BB = q1(:,I);
      for K = 1:4
         plot(p_aux(1,BB(K)),p_aux(2,BB(K)),'r.'), hold on
         %pause(0.1)
      end
      plot(p_aux(1,BB),p_aux(2,BB),'w.'), hold on
     % pause(0.1)
   end
end
% -- Testen Zwischenpunkte fuer Randstuecke
p3 = [p,p1,p2];
for I = 1:size(e,2)
   AA = e(1:2,I);
   plot(p(1,AA),p(2,AA),'k','linewidth',2), hold on
   BB = e(6,I);
      plot(p3(1,BB),p3(2,BB),'r*'), hold on
      pause(0.1)
   end
end
