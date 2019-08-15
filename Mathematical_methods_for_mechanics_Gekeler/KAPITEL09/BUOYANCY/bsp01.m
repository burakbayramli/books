function [p,e,t] = bsp01;
% Thermal flow in cup; cf.Ninomiya/Onishi
% geometry adapted to that in bsp01g.m 
% which must be chosen in that form for initmesh.m

p1 = [3  3    2.83 3.17  3     2.67  3.33  2.67 3.33  3    2.42 3.58;
      0  0.23 0    0     0.69  0.23  0.23  0    0     1.15 0.69 0.69];
p2 = [2.33  3.67 2.5 3.5 3    2.25 3.75 1.83 4.17 2    4    3;
      0.23  0.23 0   0   1.62 1.15 1.15 0.69 0.69 0.23 0.23 2.31];
p3 = [2.12 3.88 1.5  4.5  1.25 4.75  3  2.03 3.98 1.23 4.77 0.75;
      1.62 1.62 1.15 1.15 0.69 0.69  3  2.31 2.31 1.62 1.62 1.15];
p4 = [5.25 2 4 1.05 4.95 0.35 5.65 1 5 0.08 5.93 0  6;
      1.15 3 3 2.31 2.31 1.62 1.62 3 3 2.31 2.31 3  3];
p = [p1,p2,p3,p4];

t1 = [31    38    48    31    38    48    24    32    40    24    32    40;
      38    44    40    32    40    46    32    40    46    25    34    42;
      32    40    44    24    32    40    25    34    42    17    25    34];
t2 = [17    25    34    17    25    34    10    18    27    10    18    27;
      25    34    42    18    27    36    18    27    36    11    20    29;
      18    27    36    10    18    27    11    20    29     5    11    20];
t3 = [ 5    11    20     5    11    20     2     6    13     2     6    13;
      11    20    29     6    13    22     6    13    22     3     8    15;
       6    13    22     2     6    13     3     8    15     1     3     8];
t4 = [45    45    39    41    39    31    47    41    33    41    33    24;
      41    39    31    47    33    24    41    33    24    35    26    17;
      49    41    33    49    41    33    43    35    26    43    35    26];
t5 = [43    35    26    35    26    17    37    28    19    28    19    10;
      35    26    17    28    19    10    28    19    10    21    12     5;
      37    28    19    37    28    19    30    21    12    30    21    12];
t6 = [30    21    12    21    12     5    23    14     7    14     7     2;
      21    12     5    14     7     2    14     7     2     9     4     1;
      23    14     7    23    14     7    16     9     4    16     9     4];
t = [t1,t2,t3,t4,t5,t6];
% triangles associated to subregions
J = find(p(1,:) > 3);
t4 = 2*ones(1,size(t,2));
for I = 1:size(t,2)
    if ~isempty(intersect(t(1:3,I),J))
    t4(I) = 1;
    end
end    
t = [t;t4];  
e1 = [ 1 4 9;
       4 9 16];
LE = size(e1,2); AUX = linspace(0,1,LE + 1);      
e1 = [e1;AUX(1:end-1);AUX(2:end);1*ones(1,LE);
      1*ones(1,LE);zeros(1,LE)];


e2 = [16 23 30 37 43 47;
      23 30 37 43 47 49];
LE = size(e2,2); AUX = linspace(0,1,LE + 1);      
e2 = [e2;AUX(1:end-1);AUX(2:end);2*ones(1,LE);
      ones(1,LE);zeros(1,LE)];
  
e3 = [49 45 39;
      45 39 31];
LE = size(e3,2); AUX = linspace(0,1,LE + 1);      
e3 = [e3;AUX(1:end-1);AUX(2:end);3*ones(1,LE);
      ones(1,LE);zeros(1,LE)];

e4 = [31 24 17 10 5 2;
      24 17 10  5 2 1];
LE = size(e4,2); AUX = linspace(0,1,LE + 1);      
e4 = [e4;AUX(1:end-1);AUX(2:end);4*ones(1,LE);
      ones(1,LE);2*ones(1,LE)];

e5 = [31 38 44;
      38 44 48];
LE = size(e5,2); AUX = linspace(0,1,LE + 1);      
e5 = [e5;AUX(1:end-1);AUX(2:end);5*ones(1,LE);
      2*ones(1,LE);zeros(1,LE)];

e6 = [48  46 42 36 29 22;
      46  42 36 29 22 15];
LE = size(e6,2); AUX = linspace(0,1,LE + 1);      
e6 = [e6;AUX(1:end-1);AUX(2:end);6*ones(1,LE);
      2*ones(1,LE);zeros(1,LE)];
      
e7 = [15 8 3 ;
      8  3 1 ];
LE = size(e7,2); AUX = linspace(0,1,LE + 1);      
e7 = [e7;AUX(1:end-1);AUX(2:end);7*ones(1,LE)
      2*ones(1,LE);zeros(1,LE)];


e  = [e1,e2,e3,e4,e5,e6,e7];

 