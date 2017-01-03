clear;
load('inputDataOHLCDaily_20120517', 'syms', 'tday', 'cl');

idxV=find(strcmp('VX', syms));
idxE=find(strcmp('ES', syms));

VX=cl(:, idxV);
tdayV=tday(:, idxV);

ES=cl(:, idxE);
tdayE=tday(:, idxE);

[tday idxV idxE]=intersect(tdayV, tdayE);
VX=VX(idxV);
ES=ES(idxE);

scatter(VX, ES);

post200808=find(tday>=20080801);

hedgeRatio=regress(50*ES(post200808), [1000*VX(post200808) ones(length(post200808), 1)]);