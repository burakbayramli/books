function pp2 = projectionpl(p1, d, p2)
% projectionpl(p1, d, p2)
%
% p1, d and p2 are vectors. Computes the projection of the point p2
% onto the line defined by p1 and d.


  p2rel = p2 - p1;

  pp2 = p1 + projectionvv(d, p2rel);
