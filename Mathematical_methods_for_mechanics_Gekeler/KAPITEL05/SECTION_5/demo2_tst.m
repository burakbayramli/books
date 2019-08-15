function demo3
% Programm zur Fortsetzung NACH Hopf-Bifurkation
% - omega*u' + A*u + mu*B*u + f(mu,u) = 0 in R^p, u(0) = u(2*pi)
% ACHTUNG: Zuerst DEMO1.M Aufrufen!!
% Fortsetzung nur fuer Rueckwaertsdifferenzen !!!!
% FUNCTIONS: bdf.m, cg_lq.m
% USER DEFINED FUNCTIONS: bsp0x.m
%
% WIE DEMO2, Bsp. 1, aber 3D-Bild
clear, clc, format short, format compact
errorcode = 0;
disp(' Zuerst DEMO1 Aufrufen!')
%-------------------------------------------------
   load datenB1 Y omga mu Parmeter
   tol     = 1E-8; % Toleranz
   maxit   = 20;   % Max. Schrittzahl im Newtonverfahren
   Parcont = [tol, maxit, omga, mu];
   OMGA = omga;
   XX = Y(1,:); YY = Y(2,:);
   MU  = [mu, 0.1, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2];
   for I = 2:length(MU)
      mu = MU(I);
      Parcont = [tol, maxit, omga, mu];
      [U,omga,errorcode] = hopf_contin('bsp01',Y,Parcont,Parmeter);
      Y = U;
      XX = [XX;U(1,:)]; YY = [YY;U(2,:)]; OMGA = [OMGA; omga];
      omga_iter = [omga, I]
      disp('-------------------------')
   end
   save datenC2 XX YY OMGA MU
   %bild050604a
