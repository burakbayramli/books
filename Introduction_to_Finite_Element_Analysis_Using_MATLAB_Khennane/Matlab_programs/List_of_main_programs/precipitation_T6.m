%   PROGRAM  precipitation_T6.M
%
%   This program estimates the quatity of rainfall over 
%   an area discretised with linear triangular elements
%
clear
clc
nel = 1    % Total number of elements
nnd = 6    % Total number of nodes
nne = 6    % Number of nodes per element
npt=7;
samp=hammer(npt);
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
connec = [1   2   3    4    5    6]; ... % Element 1
%
AT = 0. ; % Initialise total area to zero
QT = 0. ; % Initialise total rainfall to zero
%
for i=1:nel
%
%   for each element retrieve the vector qe containing the precipitations 
%   at its nodes as well as the matrix coord containing 
%   the x and y coordinates of its nodes
%   
    for k=1: nne
        qe(k) = q(connec(i,k));
        for j=1:2
        coord(k,j)=geom(connec(i,k),j);
        end
    end

    for ig = 1:npt
        WI = samp(ig,3);
        [der,fun] = fmT6_quad(samp, ig);   
        JAC = der*coord;
        DET = det(JAC);
%
%   calculate its area
%  
    AT = AT+ WI*DET;
% 
%  Estimate quatity of rain over its area
% 
   QT = QT + WI*dot(fun,qe)*DET;
    end
 end   
% 
AT
QT
    