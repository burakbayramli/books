function drawaddition(v1, v2)
% drawaddition(v1, v2)
%
% v1 and v2 are vectors. Draws the addition process of v1 + v2.


initgraphics;
cleargraphics;
drawaxes;

s = v1 + v2;

drawvector(v1);
drawvector(v2);
drawvector(s);

drawarrow(v1, s);
drawarrow(v2, s);
