function [p,e,segnr1,segnr2] = bsp01
% Buchstaben FEM
% p : geordnete Knoten des Randes
% e : Rand in der ueblichen Bezeichnung
%     Aussenrand im math. positiven Sinn geordnet
%     ev. Innenrand im math. negativen Sinn geordnet
% Segnr1: Segnrn des aeusseren Randes (pos. geordnet)
% Segnr2: Segnrn des inneren Randes  (neg. geordnet)
%
p = [...
 0  0   ; 1.5 0   ; 3    0   ;

 3  2.75; 3    5.5 ; 5  6.5 ; 5  8.75; 5   11  ; 3    12  ;
 3  15  ; 6    15  ; 6  12 ;  6  8.75; 6   5.5 ; 6    2.75;
 6  0   ; 9    0   ; 12 0 ;   15 0   ; 15  2.5 ; 15   5   ;
 15 7.5 ; 15   9.5 ; 17 9.5 ; 17 7.5 ; 17  5   ; 17   2.5 ;
 17 0   ; 18.5 0   ; 20 0   ;
 20 2.5 ; 20 5   ; 20  7.5 ; 20   10  ; 20 12.5; 20   15  ;
 20 17.5; 20 20  ; 17  20  ; 16.5 16.5; 16 13  ; 15.5 16.5;
 15 20  ; 12 20  ; 12  15  ; 12   12  ; 12 9   ; 12   5.5 ;
 9  5.5 ; 11 6.5 ; 11  8.75; 11   11  ; 9  12  ; 9    15  ;
 11 15 ;  11 20  ; 8.5 20  ; 6    20  ; 3  20  ; 0    20  ;
 0  15 ;  0 12   ; 0   8.75; 0    5.5 ; 0  3  ]; % 65 Knoten
p = p';
e1 = [1, 2;
      2, 3];
e2 = [[3:27];[4:28]]; L2 = size(e2,2);
e3 = [28, 29;
      29, 30];
e4 = [[30:64];[31:65]]; e4 = [e4,[65;1]]; L4 = size(e4,2);
e1 = [e1;zeros(2,2);ones(1,2)];
e2 = [e2;zeros(2,L2);2*ones(1,L2)];
e3 = [e3;zeros(2,2);3*ones(1,2)];
e4 = [e4;zeros(2,L4);4*ones(1,L4)];
e = [e1, e2, e3, e4];
segnr1 = [1,2,3,4]; segnr2 = [];
