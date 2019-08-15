% De Casteljau - Algorithmus
% function p = decastel(b,x);
%
% berechnet Bezier-Polygon p (auf [0,1]) gegeben durch Bezier-Punkte b 
% an den Stellen x mit Hilfe des de Casteljau-Algorithmus
%
% input:  b = [b0 ... bn] ... Bezier-Punkte
%         x = [x1 ... xm] ... Stellen, an denen p(x) berechnet wird
% output: p = [p1 ... pm] mit pk = p(xk)

% Dietrich Nowottny, 1996

function p = decastel(b,x);
n = length(b); m = length(x); % Laengen der Vektoren
x = x(:)';                    % damit x sicher ein Zeilenvektor ist
p = b(:)*ones(1,m);           % fuer jeden x-Wert eine Spalte
for k = 1:n-1                 % de Casteljau
  p = p(1:n-k,:).*(1-ones(n-k,1)*x) + p(2:n-k+1,:).*(ones(n-k,1)*x);
end
