function Y = disc_aendern(X,LR,UO)
% Zeichnet Scheibe durch Abaenderung der Vorlage X
% Knoten einfuegen und verschieben
clc, clf
axis([LR(1) LR(2) UO(1) UO(2)])
axis equal, axis manual, grid on,hold on
Y = X;
% Knoten einfuegen -----------------------
but = 1;
while but == 1
   set(gca,'nextplot','replacechildren');
   plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
   plot(Y(1,:),Y(2,:),'.','markersize',12)
   text(LR(1),UO(2),'Knoten Einfuegen, Ende mit re. Mouse-Taste')
   [U,V,but]     = ginput(1);
   if but == 1
      DIST      = Y - [U;V]*ones(1,size(Y,2));
      DIST      = sqrt(DIST(1,:).*DIST(1,:) + DIST(2,:).*DIST(2,:));
      J         = find(DIST == min(DIST));
      Z         = [Y(:,1:J),[U; V],Y(:,J+1:size(Y,2))];
      set(gca,'nextplot','replacechildren');
      plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
      plot(Z(1,:),Z(2,:),'.','markersize',12)
      text(LR(1),UO(2),'Accept mit Li, Loeschen mit Re')
      [U,V,but1] = ginput(1);
      if but1 == 1, Y = Z; %save daten_aux Y
      end
   end
end
% -- Knoten verschieben --------------------
but = 1;
while but == 1
   set(gca,'nextplot','replacechildren');
   plot(Y(1,:),Y(2,:),'k','linewidth',2), hold on
   plot(Y(1,:),Y(2,:),'.','markersize',12)
   text(LR(1),UO(2),'Knoten Verschieben, Ende mit re. Mouse-Taste')
   [U,V,but]     = ginput(1);
   if but == 1
      DIST      = Y - [U;V]*ones(1,size(Y,2));
      DIST      = sqrt(DIST(1,:).*DIST(1,:) + DIST(2,:).*DIST(2,:));
      J         = find(DIST == min(DIST));
      Z = Y; Z(:,J) = [U;V];
      set(gca,'nextplot','replacechildren');
      plot(Z(1,:),Z(2,:),'k','linewidth',2), hold on
      plot(Z(1,:),Z(2,:),'.','markersize',12)
      text(LR(1),UO(2),'Accept mit Li, Loeschen mit Re')
      [U,V,but1] = ginput(1);
      if but1 == 1, Y = Z; %save daten_aux Y
      end
   end
end
text(LR(1),(UO(1)+UO(2))/2,'Fertig, Taste Druecken!','fontsize',20)
pause
close
