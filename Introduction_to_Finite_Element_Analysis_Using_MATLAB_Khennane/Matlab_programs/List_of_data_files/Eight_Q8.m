% Eight elements mesh
%
global geom connec nel nne nnd  RI RE
nnd = 37        % Number of nodes
%
% The matrix geom contains the x and y coordinates of the nodes
%
geom = [RI              0.;                                        ...  % x and y (node 1)
        RI+(RE-RI)/4    0.;                                        ...  % x and y (node 2)
        RI+(RE-RI)/2    0.;                                        ...  % x and y (node 3)
        RI+3*(RE-RI)/4  0.;                                        ...  % x and y (node 4)
        RE              0.;                                        ...  % x and y (node 5)
%
        RI*cos(pi/16)   RI*sin(pi/16);                             ...  % x and y (node 6)
        (RI+(RE-RI)/2)*cos(pi/16)  (RI+(RE-RI)/2)*sin(pi/16);      ...  % x and y (node 7)
        RE*cos(pi/16)   RE*sin(pi/16);                             ...  % x and y (node 8)
%
         RI*cos(pi/8)   RI*sin(pi/8);                              ...  % x and y (node 9)
        (RI+(RE-RI)/4)*cos(pi/8)  (RI+(RE-RI)/4)*sin(pi/8);        ...  % x and y (node 10)
        (RI+(RE-RI)/2)*cos(pi/8)  (RI+(RE-RI)/2)*sin(pi/8);        ...  % x and y (node 11)
        (RI+3*(RE-RI)/4)*cos(pi/8)  (RI+3*(RE-RI)/4)*sin(pi/8);    ...  % x and y (node 12)
         RE*cos(pi/8)  RE*sin(pi/8);                               ...  % x and y (node 13)
%        
         RI*cos(3*pi/16)   RI*sin(3*pi/16);                        ...  % x and y (node 14)
        (RI+(RE-RI)/2)*cos(3*pi/16)  (RI+(RE-RI)/2)*sin(3*pi/16);  ...  % x and y (node 15)
         RE*cos(3*pi/16)   RE*sin(3*pi/16);                         ...  % x and y (node 16)
%
         RI*cos(pi/4)   RI*sin(pi/4);                              ...  % x and y (node 17)
        (RI+(RE-RI)/4)*cos(pi/4)  (RI+(RE-RI)/4)*sin(pi/4);        ...  % x and y (node 18)
        (RI+(RE-RI)/2)*cos(pi/4)  (RI+(RE-RI)/2)*sin(pi/4);        ...  % x and y (node 19)
        (RI+3*(RE-RI)/4)*cos(pi/4)  (RI+3*(RE-RI)/4)*sin(pi/4);    ...  % x and y (node 20)
         RE*cos(pi/4)  RE*sin(pi/4);                               ...  % x and y (node 21)
%        
         RI*cos(5*pi/16)   RI*sin(5*pi/16);                        ...  % x and y (node 22)
        (RI+(RE-RI)/2)*cos(5*pi/16)  (RI+(RE-RI)/2)*sin(5*pi/16);  ...  % x and y (node 23)
         RE*cos(5*pi/16)   RE*sin(5*pi/16);                         ...  % x and y (node 24)    
%
         RI*cos(6*pi/16)   RI*sin(6*pi/16);                              ...  % x and y (node 25)
        (RI+(RE-RI)/4)*cos(6*pi/16)  (RI+(RE-RI)/4)*sin(6*pi/16);        ...  % x and y (node 26)
        (RI+(RE-RI)/2)*cos(6*pi/16)  (RI+(RE-RI)/2)*sin(6*pi/16);        ...  % x and y (node 27)
        (RI+3*(RE-RI)/4)*cos(6*pi/16)  (RI+3*(RE-RI)/4)*sin(6*pi/16);    ...  % x and y (node 28)
         RE*cos(6*pi/16)  RE*sin(6*pi/16);                               ...  % x and y (node 29)    
 %
         RI*cos(7*pi/16)   RI*sin(7*pi/16);                        ...  % x and y (node 30)
        (RI+(RE-RI)/2)*cos(7*pi/16)  (RI+(RE-RI)/2)*sin(7*pi/16);  ...  % x and y (node 31)
         RE*cos(7*pi/16)   RE*sin(7*pi/16);                         ...  % x and y (node 32) 
 %
         RI*cos(pi/2)   RI*sin(pi/2);                              ...  % x and y (node 33)
        (RI+(RE-RI)/4)*cos(pi/2)  (RI+(RE-RI)/4)*sin(pi/2);        ...  % x and y (node 34)
        (RI+(RE-RI)/2)*cos(pi/2)  (RI+(RE-RI)/2)*sin(pi/2);        ...  % x and y (node 35)
        (RI+3*(RE-RI)/4)*cos(pi/2)  (RI+3*(RE-RI)/4)*sin(pi/2);    ...  % x and y (node 36)
         RE*cos(pi/2)  RE*sin(pi/2)]                                   % x and y (node 37)    
%     
nel = 8          % Number of elements
nne = 8         % Number of nodes per element
%
% The matrix connec contains the connectivity of the elements
%
connec = [1    2    3    7    11    10    9     6; ...   % Element 1
          3    4    5    8    13    12    11    7; ...   % Element 2 
          9    10   11   15   19    18    17   14; ...   % Element 3
         11    12   13   16   21    20    19   15; ...   % Element 4
         17    18   19   23   27    26    25   22; ...   % Element 5 
         19    20   21   24   29    28    27   23; ...   % Element 6 
         25    26   27   31   35    34    33   30; ...   % Element 7 
         27    28   29   32   37    36    35   31];...   % Element 8   
%
% End script Eight_Q8.m