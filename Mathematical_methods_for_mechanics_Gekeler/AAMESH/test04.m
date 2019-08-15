function test04
% Testen von Viereckelementen

clear, clc, format short, format compact
% Das Beispiel:
FF1 = 'bsp008'; FF2 = 'bsp008g'; FF3 = 'bsp008h';

REFINE = 1; % Anzahl Gitterverfeinerungen
tt = 0.01; 
% ------------------------------
[p,e,q] = feval(FF1); 
for I = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t,q] = mesh01_tq(FF2,p,e,[],q);
end
[pp,p1,e,t1,q1] = mesh06_tq(p,e,[],q);
clf, hold on
for I = 1:size(e,2)
    A = [p(1,e(1,I));p(1,e(2,I))];
    B = [p(2,e(1,I));p(2,e(2,I))];
    plot(A,B,'r'), hold on
end
axis equal, axis manual, grid on
mesh36(p,q,'b'), hold on

flag = 1;
if flag == 1
% -- Testen Zwischenpunkte fuer Parallelogrammelemente
if ~isempty(q)
   p_aux = [p,p1];
   for I = 1:size(q,2)
      AA = q(:,I); AA = [AA;AA(1)];
      fill(p(1,AA),p(2,AA),'g'), hold on
      BB = q1(:,I);
      for K = 1:4
         plot(p_aux(1,BB(K)),p_aux(2,BB(K)),'r.'), hold on
         pause(0.03)
      end
      plot(p_aux(1,BB),p_aux(2,BB),'w.'), hold on
      pause(0.03)
   end
end
% -- Testen Zwischenpunkte fuer Randstuecke
p3 = [p,p1];
for I = 1:size(e,2)
   AA = e(1:2,I);
   plot(p(1,AA),p(2,AA),'k','linewidth',2), hold on
   BB = e(6,I);
      plot(p3(1,BB),p3(2,BB),'r*'), hold on
      pause(0.1)
   end
end
end
