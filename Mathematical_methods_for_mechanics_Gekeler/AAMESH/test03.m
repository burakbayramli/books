function test03
% Testen quadratische Dreieckselemente
% und quadratische Parallelogrammelemente
% partielle Verfeinerung

clear, clc, format short, format compact
tt = 0.1;
% ------------------------------
[p,e,t,q] = bsp100;
clf, hold on
X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
if ~isempty(t)
   trimesh(t',X,Y,Z), hold on
end
mesh36(p,q,'b'), hold on
axis([-2 2 -1 3]), grid on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))]; B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
   %   pause
end
%
REFINE = 1;
for I = 1:REFINE
   disp(' Refinemesh ')
   it = [1,2]; iq = [1,2];
   [p,e,t,q] = mesh01_tqq([],p,e,t,q,iq);
   for I = 1:size(e,2)
      A = [p(1,e(1,I)),p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
   end
   X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
   trimesh(t',X,Y,Z,'edgecolor','r'), hold on
   mesh36(p,q,'b'), hold on
   pause 
   [p,e,t,q] = mesh01_qtt([],p,e,t,q,it);
   X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
   trimesh(t',X,Y,Z,'edgecolor','r'), hold on
   mesh36(p,q,'b'), hold on
   pause 
end
%p  = mesh10_tq(p,e,t,q,3); % Jigglemesh
%t = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen

[p1,p2,e,t1,q1]  = mesh06_tq(p,e,t,q);
clf, hold on

X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
trimesh(t',X,Y,Z,'edgecolor','r'), hold on
mesh36(p,q,'b'), hold on
axis([-2 2 -1 3]), grid on

for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))]; B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'k','linewidth',2), hold on
   %pause
end

flag = 1;
if flag == 1
% -- Testen Zwischenpunkte fuer Dreiecke
if ~isempty(t)
   for I = 1:size(t,2)
      AAA = t(:,I); AAA = [AAA;AAA(1)];
      fill(p(1,AAA),p(2,AAA),'y'), hold on
      AA = t1(:,I) - size(p,2); %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      for K = 1:3
         plot(p1(1,AA(K)),p1(2,AA(K)),'r.'), hold on
        % pause(tt)
      end
      plot(p1(1,AA),p1(2,AA),'w.'), hold on
     % pause(tt)
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
        % pause(tt)
         % pause
      end
      plot(p_aux(1,BB),p_aux(2,BB),'w.'), hold on
     % pause(tt)
      % pause
   end
end
pause
% -- Testen Zwischenpunkte fuer Randstuecke
p3 = [p,p1,p2];
for I = 1:size(e,2)
   AA = e(1:2,I);
   plot(p(1,AA),p(2,AA),'k','linewidth',2), hold on
   BB = e(6,I);
   plot(p3(1,BB),p3(2,BB),'r*'), hold on
   pause(0.5)
end


end



