function demo1
% Zeichnet Scheibe durch manuelle Eingabe
clc, clf
LR = [-5; 5]; UO = [-5;5];
axis([LR(1) LR(2) UO(1) UO(2)])
axis manual, grid on,hold on
nr = 100; KK = [0,1];
while ~ismember(nr,KK)
   nr   = input('Scheibe neu JA/NEIN (0/1)');
end
switch nr
case 0
    Y   = [];
    but = 1;
    text(LR(1),UO(1)-1,'Zum Beenden rechte Mouse-Taste')
    while but == 1
      [U,V,but]     = ginput(1);
      if but == 1
         X = [U;V]; Y = [Y,X];
      end
      plot(U,V,'.','markersize',12), hold on
   end
   Y = [Y,Y(:,1)];
   plot(Y(1,:),Y(2,:),'k','linewidth',2)
   save daten Y
case 1
   load daten Y
   % Knoten einfuegen -----------------------
   but = 1;
   while but == 1
      set(gca,'nextplot','replacechildren');
      plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
      plot(Y(1,:),Y(2,:),'.','markersize',12)
      text(LR(1),UO(1)-1,'Knoten Einfuegen, Ende mit re. Mouse-Taste')
      [U,V,but]     = ginput(1);
      if but == 1
         DIST      = Y - [U;V]*ones(1,size(Y,2));
         DIST      = sqrt(DIST(1,:).*DIST(1,:) + DIST(2,:).*DIST(2,:));
         J         = find(DIST == min(DIST));
         Z         = [Y(:,1:J),[U; V],Y(:,J+1:size(Y,2))];
         set(gca,'nextplot','replacechildren');
         plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
         plot(Z(1,:),Z(2,:),'.','markersize',12)
         text(LR(1),UO(1)-1,'Accept mit Li, Loeschen mit Re')
         [U,V,but1] = ginput(1);
         if but1 == 1
            Y = Z;
            save daten Y
         end
      end
   end
   % -- Knoten verschieben --------------------
   but = 1;
   while but == 1
      set(gca,'nextplot','replacechildren');
      plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
      plot(Y(1,:),Y(2,:),'.','markersize',12)
      text(LR(1),UO(1)-1,'Knoten Verschieben, Ende mit re. Mouse-Taste')
      [U,V,but]     = ginput(1);
      if but == 1
         DIST      = Y - [U;V]*ones(1,size(Y,2));
         DIST      = sqrt(DIST(1,:).*DIST(1,:) + DIST(2,:).*DIST(2,:));
         J         = find(DIST == min(DIST));
         Z = Y; Z(:,J) = [U;V];
         set(gca,'nextplot','replacechildren');
         plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
         plot(Z(1,:),Z(2,:),'.','markersize',12)
         text(LR(1),UO(1)-1,'Accept mit Li, Loeschen mit Re')
         [U,V,but1] = ginput(1);
         if but1 == 1
            Y = Z;
            save daten Y
         end
      end
   end
end
