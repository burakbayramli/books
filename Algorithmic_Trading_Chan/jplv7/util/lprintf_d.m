% PURPOSE: demo of lprintf() 
%          LaTeX table printing with symbols
% 
%---------------------------------------------------
% USAGE: lprintf_d
%---------------------------------------------------

% Demo program for LPRINTF

clear info
beta = 10*randn(2,3);
beta(3,:) = beta(1,:) + beta(2,:);
se = beta.*rand(3,3);
t = beta./se;
pval = abs(rand(3,3))/10;

disp('---> LPRINTF Demo  You need to hit a key at each pause.')

disp('---> Some sample data:')
beta
pval
pause

disp('---> Bare-bones output: lprintf(beta,se)')
lprintf(beta,pval);
pause

disp(' ')
disp('---> Here we change the symbols.')
info.symb = strvcat('^{*}','^{**}','^{***}');
lprintf(beta,pval,info);
pause

disp(' ')
disp('---> Now add some row/column names')
info.rname = strvcat('Market A','Market B','Market A+B');
info.cname = strvcat('Beer','Wine','Liquor');
lprintf(beta,pval,info);
pause

disp(' ')
disp('---> Now some header/footer stuff')
h1 = '\documentclass{article}';
h2 = '\usepackage{dcolumn}';
h3 = '\newcolumntype{d}{D{.}{.}{-1}}';
h4 = '\begin{document}';
h5 = '\begin{minipage}{\textwidth}';
h6 = '\begin{tabular}{lddd}';
t1 = '\end{tabular}';
t2 = '\footnotetext{$^{***,**,*}$ indicate significance at the ';
t3 = '1\%, 5\%, and 10\% levels, respecively.}';
t4 = '\end{minipage}';
t5 = '\end{document}';
info.head = strvcat(h1,h2,h3,h4,h5,h6);
info.tail = strvcat(t1,t2,t3,t4,t5);
lprintf(beta,se,info);
pause

disp(' ')
disp('---> Add some lines. info.lnnum = 2')
info.lnnum = 2;
lprintf(beta,pval,info);
pause

disp(' ')
disp('---> Last, save it to a file called test.tex')
info = rmfield(info,'cname');
newcname =  strvcat('  & \multicolumn{1}{c}{Beer} & ',...
'\multicolumn{1}{c}{Wine} & ',...
'\multicolumn{1}{c}{Liquor} \\ \hline\hline');
info.head = strvcat(info.head,newcname)
info.tail = strvcat('\hline\hline',info.tail)
lprintf(beta,pval,info,'test.tex');
disp(' ')
disp('Now LaTeX the file.  Note that it uses the DCOLUMN package.')
disp('If you do not have this package remove lines 2 & 3 of the .tex')
disp('file and change the ddd in the \begin{tabular} line to rrr')
disp('Note that the column headings are also modified for DCOLUMN')
disp('by specifying them in the info.head rather than info.cname')
