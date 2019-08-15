function [KNOTEN,ELEMENTE,RB] = bsp03a
% GEKELER: FINITE ELEMENTE --------------------------------
% Beispiel aus BATOZ et AL. S. 1798
% kubische Dreieckelemente

L = 30.48/4;    % Laenge
 H = 21.5526/4; % Breite
h = 0.3175;     % Hoehe
q = 1.8326e-2;  % Lastdichte

KNOTEN = [0,     0;     L,   0;     2*L,   0;     3*L,   0;     4*L,   0;
          H,     H;   H+L,   H;   H+2*L,   H;   H+3*L,   H;   H+4*L,   H;
          2*H, 2*H; 2*H+L, 2*H; 2*H+2*L, 2*H; 2*H+3*L, 2*H; 2*H+4*L, 2*H;
          3*H, 3*H; 3*H+L, 3*H; 3*H+2*L, 3*H; 3*H+3*L, 3*H; 3*H+4*L, 3*H;
          4*H, 4*H; 4*H+L, 4*H; 4*H+2*L, 4*H; 4*H+3*L, 4*H; 4*H+4*L, 4*H];
KNOTEN = KNOTEN';

ELEMENTE = [...
1  7  6 h q;1  2  7 h q;2  8  7 h q;2  3  8 h q;
3  9  8 h q;3  4  9 h q;4 10  9 h q;4  5 10 h q;

6 12 11 h q;6  7 12 h q;7 13 12 h q;7  8 13 h q;
8 14 13 h q;8  9 14 h q;9 15 14 h q;9 10 15 h q;

11 17 16 h q;11 12 17 h q;12 18 17 h q;12 13 18 h q;
13 19 18 h q;13 14 19 h q;14 20 19 h q;14 15 20 h q;

16 22 21 h q;16 17 22 h q;17 23 22 h q;17 18 23 h q;
18 24 23 h q;18 19 24 h q;19 25 24 h q;19 20 25 h q];

ELEMENTE = ELEMENTE';
RB = [1  1 1 1;
      2  1 1 1;
      3  1 1 1;
      4  1 1 1;
      5  1 1 1];
