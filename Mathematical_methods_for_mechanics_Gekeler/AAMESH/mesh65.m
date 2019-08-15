function AUX = mesh65(t,e)
% berechnet Randdreiecke, deren Nummer in AUX(3,:)
M = size(t,2); N = size(e,2);
AUX = [e(1:2,:); zeros(1,N)];
NUMMER = t(1:3,:);
for I = 1:N
   for K = 1:M
      M1 = find(NUMMER(:,K) == e(1,I));
      M2 = find(NUMMER(:,K) == e(2,I));
      if isempty(M1) + isempty(M2) == 0
         AUX(3,I) = K;
      end
   end
end
