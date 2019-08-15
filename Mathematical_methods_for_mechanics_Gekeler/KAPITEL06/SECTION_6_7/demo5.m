function demo5
% Generates movie for top

clc, clf, clear
%set(gcf,'renderer','zbuffer')
set(gcf,'renderer','painter');
m = 10; n = 40; h = 2; r = 1;
% -- Kegel mit z-Achse als Symmetrieachse ----
[X,Y,Z1,Z2] = kegel(m,n,h,r);
% -- Kegel drehen -----------------
A = [0;-1;0]; phi = pi/4;
[XN,YN,ZN] = drehen(X,Y,Z1,A,phi);
[XM,YM,ZM] = drehen(X,Y,Z2,A,phi);
save daten XN YN ZN XM YM ZM
% -- Film ----------------------------
%clear
colormap(gray);
Drehmovie = 1
if Drehmovie == 1
   clf;
   load daten XN YN ZN XM YM ZM
   Achse = [0;0;1];
   N     = 100;  %     Anzahl Bilder
   PSI   = linspace(0,2*pi,N);
   %FILM  = moviein(N);
   axis([-2 2 -2 2 -2 2]);
   axis equal
   axis manual;
   grid on
   xlabel('x'), ylabel('y'), zlabel('z')
   hold on
   for I = 1:N
      phi = PSI(I);
      [XN1,YN1,ZN1] = drehen(XN,YN,ZN,Achse,phi);
      [XM1,YM1,ZM1] = drehen(XM,YM,ZM,Achse,phi);
      surf(XN1,YN1,ZN1); hold on
      surf(XM1,YM1,ZM1);
      FILM(I) = getframe;
      set(gca,'nextplot','replacechildren');
   end
   save datenfilm FILM
end

