function [p,e,t,q] = bsp001b(Fall)
% Quadrat mit Loch, 4. Quadrant, grobes Gitter
% Schwarz: beisp66.dat
% Geometriedaten: fuer Dreiecke (Fall = 1)
% oder fuer Dreiecke und Vierecke (Fall = 2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fall = 2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
KNOTEN = [...
 0 0  ;  1   0       ;  0  1 ;  1       1  ;
 2 0  ;  0   2       ;  1  2 ;  2.17157 1.0;
 1.8 2;  0   3       ;  1  3 ;  2.6     1.8;
 0 4  ;  2   3       ;  1  4 ;  0       5  ;
 1 5  ;  3.2 2.4     ;  2  4 ;  2       5  ;
 3 3.2;  4.0 2.82843 ;  3  4 ;  3       5  ;
 5 3  ;  4   4       ;  4  5 ;  5       4  ;  5 5 ];
p = KNOTEN';

% Dreieckselemente
ELEMENTE1 = [...
 2  5  4 ;  4   5  8 ;  4  8  9 ;  4  9  7 ;  8 12  9 ;
 7  9 11 ;  9  14 11 ;  9 12 14 ; 12 18 14 ; 14 18 21 ;
14 21 19 ; 18  22 21 ; 19 21 23 ; 21 22 26 ; 21 26 23 ;
22 25 26 ; 25  28 26 ];
t1 = ELEMENTE1';
% Quadrate
ELEMENTE2 = [...
 1  2  4  3;  3  4  7  6;  6  7 11 10; 10 11 15 13;
11 14 19 15; 13 15 17 16; 15 19 20 17; 19 23 24 20;
23 26 27 24; 26 28 29 27];
q = ELEMENTE2';
% Dreiecke aus Quadraten
ELEMENTE3 = [...
 1  2  4 ;  4  3  1;  3  4  7 ;  7  6  3;
 6  7 11 ; 11 10  6; 10 11 15 ; 15 13 10;
11 14 19 ; 19 15 11; 13 15 17 ; 17 16 13;
15 19 20 ; 20 17 15; 19 23 24 ; 24 20 19;
23 26 27 ; 27 24 23; 26 28 29 ; 29 27 26];
t3 = ELEMENTE3';
%
e1 = [1 2; 2 5];                      L1 = size(e1,2);
e2 = [5 8 12 18 22; 8 12 18 22 25];   L2 = size(e2,2);
e3 = [25 28; 28 29];                  L3 = size(e3,2);
e4 = [29 27 24 20 17; 27 24 20 17 16];L4 = size(e4,2);
e5 = [16 13 10 6 3; 13 10 6 3 1];     L5 = size(e5,2);

AUX1 = [0,[1:L1-1]]/L1; AUX2 = [1:L1]/L1;
e1 = [e1;AUX1;AUX2;ones(1,L1)];
AUX1 = [0,[1:L2-1]]/L2; AUX2 = [1:L2]/L2;
e2 = [e2;AUX1;AUX2;2*ones(1,L2)];
AUX1 = [0,[1:L3-1]]/L3; AUX2 = [1:L3]/L3;
e3 = [e3;AUX1;AUX2;3*ones(1,L3)];
AUX1 = [0,[1:L4-1]]/L4; AUX2 = [1:L4]/L4;
e4 = [e4;AUX1;AUX2;4*ones(1,L4)];
AUX1 = [0,[1:L5-1]]/L5; AUX2 = [1:L5]/L5;
e5 = [e5;AUX1;AUX2;5*ones(1,L5)];
e = [e1,e2,e3,e4,e5];
% -- Waehlen : -------------------------
switch Fall
case 1
   t = [t1,t3]; q = [];
case 2
   t = t1;
end 