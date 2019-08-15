function shoal(p,e,t,shallow);
% calculates shoal

TAUX = 0*t(1:3,:); LE = length(shallow);
if LE
   for I = 1:LE
      J = find(t(1,:) == shallow(I));
      if ~isempty(J), TAUX(1,J) = 1; end
      J = find(t(2,:) == shallow(I));
      if ~isempty(J), TAUX(2,J) = 1; end
      J = find(t(3,:) == shallow(I));
      if ~isempty(J), TAUX(3,J) = 1; end
   end
   SUM = sum(TAUX);
   J = find(SUM == 3);
   ts = t(1:3,J);  LTS = size(ts,2);
   if LTS
      for I = 1:LTS
         K = ts(:,I);
         fill(p(1,K),p(2,K),'y','erasemode','none'), hold on
      end
   end
end
