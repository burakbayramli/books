function pv2 = projectionvv(v1, v2)
% projectionvv(v1, v2)
%
% v1 and v2 are vectors. Computes the projection of v2 on v1.

pv2 = dot(v2, v1) / dot(v1, v1) * v1;
