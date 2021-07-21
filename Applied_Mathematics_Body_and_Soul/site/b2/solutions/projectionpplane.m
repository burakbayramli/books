function pp2 = projectionpplane(p1, n, p2)
% projectionpplane(p1, n, p2)
%
% p1, n and p2 are vectors. Computes the projection of p2 onto the
% plane defined by p1 and n.


  p2rel = p2 - p1;

  pp2 = p2 - projectionvv(n, p2rel);
