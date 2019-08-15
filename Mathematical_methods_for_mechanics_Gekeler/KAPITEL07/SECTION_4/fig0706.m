function bld070402
% Zeichnet ebenes Stabwerk, Beispiel 2
disp(' Zuerst DEMO1.M, Beispiel 2 Aufrufen ')
load daten1 p e STABKRAEFTE LAGERKRAEFTE LASTEN
clf, hold on
c = 0.3;   %Laenge der Pfeilspitze
d = 0.05;  % Breite der Pfeilspitzenbasis
RR = 0.05; % Radius von CIRCLE.M
for I = 1:size(e,2)
   if STABKRAEFTE(I) > 0
  h = plot(p(1,e(:,I)),p(2,e(:,I)),'r','linewidth',2); hold on
   end
   if STABKRAEFTE(I) < 0
  k = plot(p(1,e(:,I)),p(2,e(:,I)),'b--','linewidth',2); hold on
   end
   if STABKRAEFTE(I) ==  0
      plot(p(1,e(:,I)),p(2,e(:,I)),'g:','linewidth',2), hold on
   end
end
legend([h,k],'tension','pression',1)
% -- Zeichne normierte Lagerkraefte ------
for I = 1:size(LAGERKRAEFTE,2)
    NORM = norm(LAGERKRAEFTE(2:3,I));
    if NORM ~= 0
       LAGERKRAEFTE(2:3,I) = LAGERKRAEFTE(2:3,I)/NORM;
    end
end
LGA = p(:,LAGERKRAEFTE(1,:)); LGB = LGA + LAGERKRAEFTE(2:3,:);
myquiver(LGA,LGB,c,d,'g',2,1)
% -- Zeichne normierte Lasten -----------
N    = size(p,2);
LSTN = zeros(2,N);
for I = 1:N
    NORM = norm(LASTEN(:,I));
    if NORM ~= 0, LSTN(:,I) = LASTEN(:,I)/NORM; end
end
PA = p; PB = PA + LSTN;
myquiver(PA,PB,c,d,'k',2,2)
for I = 1:size(p,2)
   circle(p(1,I),p(2,I),RR,'w')
end
axis equal
LU = [-1.5,-1.5];
plot(LU(1),LU(2),'w.'), hold on
RO = [6.2,1.2];
plot(RO(1),RO(2),'w.'), hold on

axis equal tight
text(-0.6,-0.35,'L1','fontsize',18)
text(-0.6,0.5,'L2','fontsize',18)
text(5.4,0.5,'L3','fontsize',18)
text(1.4,-0.5,'F1','fontsize',18)
text(5.1,1,'F2','fontsize',18)

%grid on
clear
