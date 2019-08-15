function fig0705_06
% Figures 7.5 and 7.6, plots plane framework

disp(' Call first demo1, if not done ! ')
load daten1 p e STABKRAEFTE LAGERKRAEFTE LASTEN PARMETER
PHI = PARMETER(4);
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
X = [-0.25*cos(PHI),cos(PHI)]; Y = [2-0.25*sin(PHI),2 + sin(PHI)];
plot(X,Y,'k--'), hold on
X = [0, 1]; Y = [2, 2];
plot(X,Y,'--k'), hold on
axis equal
text(3.1,1,'P1','fontsize',18)
text(2.2,0,'P2','fontsize',18)
text(1.1,1.2,'P3','fontsize',18)
text(-0.45,0,'P4','fontsize',18)
text(-0.5,1.9,'P5','fontsize',18)
text(2.6,0.4,'S1','fontsize',18)
text(1.9,1.2,'S2','fontsize',18)
text(1.6,0.6,'S3','fontsize',18)
text(0.9,-0.25,'S4','fontsize',18)
text(0.6,0.4,'S5','fontsize',18)
text(0.5,1.7,'S6','fontsize',18)
text(-0.45,1,'S7','fontsize',18)
text(3.1,0,'F_1','fontsize',18)
text(0.8,2.2,'\phi','fontsize',18)
text(0.9,2.5,'SUPPORT','fontsize',18)
text(0.38,-0.25,'L_1','fontsize',18)
text(-0.35,-0.45,'L_2','fontsize',18)
text(0,2.5,'L_3','fontsize',18)

axis equal
%grid on
clear
