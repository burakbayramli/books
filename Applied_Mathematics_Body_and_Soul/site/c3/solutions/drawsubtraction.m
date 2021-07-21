function drawsubtraction(v1, v2)

initgraphics;
cleargraphics;
drawaxes;

s = v1 - v2;

drawvector(v1);
drawvector(v2);
drawvector(s);

drawarrow(v1, s);
drawarrow(v2, s);
