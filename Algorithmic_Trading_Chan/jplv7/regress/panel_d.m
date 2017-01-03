% PURPOSE: Demonstrates use of panel data estimation
% The followig program use the data from " Introduction to the Theory and Practice of 
% Econometrics "(Judge, Hill, Griffiths, Lütkepohl and Lee) Second Edition.
% Chapter 11 pages 476-479 and pages 487-488, as an example to test the procedures for 
% panel data estimation.

clear;
%carga los datos
load datat.txt;      %Judge Example with Balanced Panel

%assign data

y= datat(:,2:3);
id = datat(:,1);

vnames =  ['c',		% Cost 
   		  'y'];		% Output     


% Pooled Estimation
results = ppooled(y);
prt_panel(results,vnames);


% Fixed Effects Estimation
result1 = pfixed(y,id);
prt_panel(result1,vnames);


% Random Effects Estimation
result2 = prandom(y,id);
prt_panel(result2,vnames);

% Haussman Test 
phaussman(result1, result2);



