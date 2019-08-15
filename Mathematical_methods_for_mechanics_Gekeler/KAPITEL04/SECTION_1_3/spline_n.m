function s = spline_n(f,x)
% natuerlicher kubischer Interpolations-Spline
% input:  f ... zu interpolierende Daten (aequidistante Stuetzstellen)
%         x ... Vektor mit x-Werten, an denen Spline ausgewertet wird
% output: s ... Werte des Spline: s_i = s(x_i)
% needs:  spl_val, decastel
% Dietrich Nowottny, 1996
m = length(f)-1;
% Berechnung der Momente d_i aus tridiagonalem LGS
M  = diag(4*ones(1,m-1)) + diag(ones(1,m-2),1) + diag(ones(1,m-2),-1);
rS = [ 6*f(2)-f(1) ; 6*f(3:m-1)' ; 6*f(m)-f(m+1) ];  % Matrix und r. Seite
d  = M\rS; d = [f(1) d' f(m+1)];                     % LGS loesen
% Berechnung der Spline_Werte mit Hilfsprogramm
s  = spl_val(f,d,x);
