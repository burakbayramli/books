% PURPOSE: An example of using mprint3
%          printing of (beta, tstat, prob) matrices
%                                                           
%---------------------------------------------------
% USAGE: mprint3_d
%---------------------------------------------------


% Demo program for MPRINT3

clear info
beta = 10*randn(2,3);
beta(3,:) = beta(1,:) + beta(2,:);
se = abs(beta.*rand(3,3));
t = beta./se;
pval = abs(rand(3,3))/10;

disp('---> MPRINT3 Demo  You need to hit a key at each pause.')

disp('---> Some sample data:')
beta
se
t
pause

disp(' ')
disp('---> Bare-bones output: mprint3(beta,se,t)')
disp(mprint3(beta,se,t))
pause

disp(' ')
disp('---> With just two matrices: mprint3(beta,se)')
disp(mprint3(beta,se))
pause

disp(' ')
disp('---> Here we change the formatting.')
info.bfmt = strvcat('%10.6f','%7.4f','%5.0f');
info.sefmt = '[%8.4f]';
info.tfmt ='%8.4f';
disp(mprint3(beta,se,t,info))
pause

disp(' ')
disp('---> Now add some extra row spacing:  info.vspc = 3;')
info.vspc = 3;
disp(mprint3(beta,se,t,info))
pause

disp(' ')
disp('---> How about some more column spacing?  info.hspc = 10;')
info.hspc = 10;
disp(mprint3(beta,se,t,info))
pause

disp(' ')
disp('---> And finally some LaTeX output: info2.ldum = 1;')
info2.ldum = 1;
disp(mprint3(beta,se,t,info2))