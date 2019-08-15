function mesh36(p,q,FARBE);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% zeichnet das Gitter fuer Viereckelemente
%clf, hold on
for I = 1:size(q,2)
   K = q(1:4,I); K = [K;K(1)];
   
   X = p(1,K); Y = p(2,K);
   fill(X,Y,'w'), hold on
end
for I = 1:size(q,2)
   K = q(1:4,I); K = [K;K(1)];
   X = p(1,K); Y = p(2,K);
   plot(X,Y,'b'), hold on
end
