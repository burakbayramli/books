% PURPOSE: Demo of vdp()
%          Estimation subject to transversal constraints
%          by means of a quadratic optimization criterion
%---------------------------------------------------
% USAGE: vdp_d
%---------------------------------------------------

close all; clear all; clc;

% Unbalanced vector 

y = [   220.00
        130.00
        200.00
        100.00
        450.00
         70.00
        120.00
        221.00 ];
     
[k,n]=size(y);
 
% Linear constraints
 
 A =[     1.00             0
          1.00             0
          1.00          1.00
          1.00             0
         -1.00             0
         -1.00             0
         -1.00             0
         -1.00         -1.00  ];
      
% VCV matrix of estimates
      
sigma=zeros(k,k);    
sigma(1,1)= 10;
sigma(2,2)=  5;
sigma(3,3)= 25;
sigma(4,4)= 55;
sigma(5,5)=  0;   % Fixed estimation --> z(5)=y(5)
sigma(6,6)= 15; 
sigma(7,7)= 10;
sigma(8,8)= 12;


% Calling van der Ploeg function    

z = vdp(y,sigma,A);

% Check

initial_discrepancy = A' * y
final_discrepancy   = A' * z

% Revision (as %)

p = 100 * ((z - y) ./ y);

% Final results:

disp ('Initial estimate, final estimate, revision in %, variances ');
results=[y z p diag(sigma)]

