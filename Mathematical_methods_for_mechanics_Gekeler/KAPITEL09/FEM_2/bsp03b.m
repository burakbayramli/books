function [KNOTEN,ELEMENTE,RB] = bsp02
% GEKELER: FINITE ELEMENTE ------------------------
% Plattenbeispiele

% Schwarz: beisp69.dat
% Rechteckplatte in achsenparalleler Lage
% Hier Dreieckelemente
% auch als Parallelogrammelemente

KNOTEN = [...
 0 0; 0 1; 0 2; 0 3; 0 4; 2 0; 2 1; 2 2; 2 3; 2 4; 4 0; 4 1; 4 2;
 4 3; 4 4; 6 0; 6 1; 6 2; 6 3; 6 4; 8 0; 8 1; 8 2; 8 3; 8 4];
 KNOTEN = KNOTEN';
ELEMENTE = [...
0.25  0    1  6  7  2;
0.25  0    2  7  8  3;
0.25  0    3  8  9  4;
0.25  0    4  9 10  5;
0.25  0    6 11 12  7;
0.25 50.0  7 12 13  8;
0.25 50.0  8 13 14  9;
0.25  0    9 14 15 10;
0.25  0   11 16 17 12;
0.25  0   12 17 18 13;
0.25  0   13 18 19 14;
0.25  0   14 19 20 15;
0.25  0   16 21 22 17;
0.25  0   17 22 23 18;
0.25  0   18 23 24 19;
0.25  0   19 24 25 20];
ELEMENTE = ELEMENTE(:,[3,4,5,1,2])'; 
RB = [...
1  1 1 1 1;
2  1 1 1 1;
3  1 1 1 1;
4  1 1 1 1;
5  1 1 1 1;
21 1 0 1 0;
22 1 0 1 0;
23 1 0 1 0;
24 1 0 1 0;
25 1 0 1 0];
