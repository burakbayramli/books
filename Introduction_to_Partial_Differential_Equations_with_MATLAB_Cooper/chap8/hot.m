


function u = hot(x,y,t)
% function which is a solution of the heat equation in the square of
% side pi, with u = 0 on three sides, and insulated on the top side.
% Answer to exercise 2) of section 7.3.

      mode1 = sin(x).*sin(y/2)*exp(-1.25*t) ;
      mode2 = sin(2*x).*sin(3*y/2)*exp(- 6.25*t);
      u = .2*mode1 +mode2 ;
