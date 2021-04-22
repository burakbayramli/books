%   PROGRAM  precipitation_T3.M
%
%   This program estimates the quatity of rainfall over 
%   an area discretised with linear triangular elements
%
nel = 4    % Total number of elements
nnd = 6    % Total number of nodes
nne = 3    % Number of nodes per element
%
% Coordinates of the rain guages (nodes)in km
%
geom = [15.     15.   ; ... %  Node 1
        62.5    25.   ; ... %  Node 2			
        110.    35.   ; ... %  Node 3			
        87.5    70.   ; ...	%  Node 4		
        65.     105.  ; ...	%  Node 5	
        40.     60.]  ;	    %  Node 6
%
% Precipiations recorded by the rain guages 
%
q = [20.; 15.; 10.; 20.; 30.; 25.] ;
%
%  Connectivity 
%
connec = [1   2   6; ... % Element 1
          2   3   4; ... % Element 2
          2   4   6; ... % Element 3
          6   4   5];    % Element 4
%
AT = 0. ; % Initialise total area to zero
QT = 0. ; % Initialise total rainfall to zero
%
for i=1:nel
%
%   for each element retrieve the x and y coordinates of its nodes
%
    xi = geom(connec(i,1),1); yi = geom(connec(i,1),2); 
    xj = geom(connec(i,2),1); yj = geom(connec(i,2),2); 
    xk = geom(connec(i,3),1); yk = geom(connec(i,3),2); 
%
%   Retrieve the precipitations recorded at its nodes
%   
    qi = q(connec(i,1)); qj =q(connec(i,2)); qk =q(connec(i,3));
%
%   calculate its area
%  
    A = (0.5)*det([1  xi   yi;...
                   1  xj   yj;...
                   1  xk   yk]);
    AT = AT + A;
% 
%  Estimate quatity of rain over its area
% 
   Q = (qi+qj+qk)*A/3;
   QT = QT + Q;
end   
% 
AT
QT
    