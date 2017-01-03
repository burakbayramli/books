%DEMODECPARTY demo for party decision problem
clear all
[party rain] = assign(1:2); % Variable order is arbitary
yes=1; no=2; % define states, starting from 1.

varinf(party).name='party'; varinf(party).domain={'yes','no'};
varinf(rain).name='rain'; varinf(rain).domain={'yes','no'};

%partial ordering: 
partialorder{1}.max=party; partialorder{2}.sum=rain;

pot(rain).variables=rain; % define array below using this variable order
pot(rain).table(yes)=0.6; pot(rain).table(no)=0.4;  

% define utilties; 
Uparty.variables=[party rain];
tmptable(yes, yes)=-100; tmptable(yes, no) = 500;
tmptable(no, yes) =0; tmptable(no, no)  = 50;
Uparty.table=tmptable;

% Sum and Max the full joint probability table (generally inefficient):
[newprob newutil] = sumpotID(pot(rain),Uparty,rain,[],partialorder,1);
disp('Raw max and sum: expected utility (no decisions made):')
disptable(newutil,varinf);
[newprob newutil] = sumpotID(pot(rain),Uparty,[],[],partialorder,0);
fprintf(1,'Optimal expected utility  = %g\n',newutil.table)