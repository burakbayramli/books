function s = spl_val(f,d,x)
% berechnet den Wert der Splines an den Stellen x
% input:  f ... zu interpolierende Daten (aequidistante Stuetzstellen)
%         d ... Vektor der d_i
%         x ... Vektor mit x-Werten, an denen Spline ausgewertet wird
% output: s ... Werte des Spline: s_i = s(x_i)
% needs:  decastel
% Dietrich Nowottny, 1996
m = length(f)-1;
% Berechnung der inneren Bezier-Punkte
b = zeros(1,3*m+1);                          % Initialisierung
b(1:3:3*m+1) = f;                            % Interpolation: b_{3k} = f_k
b(2:3:3*m-1) = (2*d(1:m)+d(2:m+1))/3;        % 3b_{3k+1} = 2d_k + d_{k+1}
b(3:3:3*m)   = (d(1:m)+2*d(2:m+1))/3;        % 3b_{3k-1} = d_{k-1} + 2d_k

% Berechnung der Werte des Splines mit de Casteljau
s = [];
for k = 0:m-1                                % Schleife ueber Segmente
  ind = find(x >= k & x < k+1);              % Werte in [k,k+1)
  x_segment = x(ind) - k;                    % Trafo auf [0,1)
  b_segment = b(3*k+1:3*k+4);                % zugehoerige Bezier-Punkte
  s_segment = spl_decastel(b_segment,x_segment); % de Casteljau-Algorithmus
  s = [s s_segment];
end
ind = find(x == m);                          % soll s an x=m ausgewertet
if ~isempty(ind)                                 % werden?
  s = [s f(m+1)];
end
