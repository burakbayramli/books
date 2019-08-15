% -- TEST.M --------------------------------------
% DEMO fuer s/w-Bild in MATLAB
% Bild mit Option "Text" und Aufloesung "sehr hoch" scannen
% Bild aus MATLAB abspeichern mit Option "-deps"
% nicht mit "-deps2" sonst PS-Fehler
% Bild wird so besser als mit Graustufen
clf
BMP = imread('lk_wapp3.bmp');
%figure('Position',[100,100,size(BMP,2),size(BMP,1)]);
image(BMP);
axis equal tight
%set(gca,'Position',[0 0 1 1]);
%hold on
%[x,y] = ginput(5);
%colormap([0 0 0;1 1 1]);
axis off;
%INFO = IMFINFO('bild03','bmp')
