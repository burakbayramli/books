% PURPOSE: An example of using xy2cont()
%          creates a contiguity matrix from
%          latittude-longitude coordinates                  
%---------------------------------------------------
% USAGE: xy2cont_d 
%---------------------------------------------------

load anselin.dat;  % Columbus neighborhood crime
xc = anselin(:,5);  % longitude coordinate
yc = anselin(:,4);  % latittude coordinate
% create contiguity matrix from x-y coordinates
[W1 W2 W3] = xy2cont(xc,yc);

fprintf(1,'non-symmetric weight matrix \n');
fprintf(1,'sum of 1st row elements = %8.2f \n',sum(full(W2(1,:))));
fprintf(1,'sum of 1st column elements = %8.2f \n',sum(full(W2(:,1))));

spyc(W2,'ok'); 
title('row-stochastic weight matrix');

fprintf(1,'symmetric non row-stochastic weight matrix \n');
fprintf(1,'sum of 1st row elements = %8.2f \n',sum(full(W1(1,:))));
fprintf(1,'sum of 1st column elements = %8.2f \n',sum(full(W1(:,1))));


