%Input Module Two_Q8.m 
%Two elements mesh
%
global geom connec nel nne nnd RI RE
nnd = 13        % Number of nodes
%
% The matrix geom contains the x and y coordinates of the nodes
%
geom = [RI              0.;                           ...  % x and y (node 1)
        RI*cos(pi/8)    RI*sin(pi/8);                 ...  % x and y (node 2)
        RI*cos(pi/4)    RI*sin(pi/4);                 ...  % x and y (node 3)
        RI*cos(3*pi/8)  RI*sin(3*pi/8);               ...  % x and y (node 4)
        RI*cos(pi/2)    RI*sin(pi/2);                 ...  % x and y (node 5)
        (RI+RE)/2     0.;                            ...  % x and y (node 6)
        ((RI+RE)/2)*cos(pi/4)  ((RI+RE)/2)*sin(pi/4); ...  % x and y (node 7)
        ((RI+RE)/2)*cos(pi/2)  ((RI+RE)/2)*sin(pi/2); ...  % x and y (node 8)
        RE              0.;                           ...  % x and y (node 9)
        RE*cos(pi/8)    RE*sin(pi/8);                 ...  % x and y (node 10)
        RE*cos(pi/4)    RE*sin(pi/4);                 ...  % x and y (node 11)
        RE*cos(3*pi/8)  RE*sin(3*pi/8);               ...  % x and y (node 12)
        RE*cos(pi/2)    RE*sin(pi/2)]                      % x and y (node 13)
        
nel = 2         % Number of elements
nne = 8         % Number of nodes per element
%
% The matrix connec contains the connectivity of the elements
%
connec = [1    6    9    10    11    7    3   2; ...   % Element 1
          3    7    11   12    13    8    5   4]       % Element 2 
%
%End of input module Two_Q8