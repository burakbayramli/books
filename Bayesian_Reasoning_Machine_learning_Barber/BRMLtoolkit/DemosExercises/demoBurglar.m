function demoBurglar
%DEMOBURGLAR demo: was it the burglar example? 

[burglar earthquake alarm radio]=assign(1:4); % Variable order is arbitary
yes=1; no=2; % define states, starting from 1.

variable(burglar).name='burglar'; variable(burglar).domain = {'yes','no'};
variable(earthquake).name='earthquake'; variable(earthquake).domain = {'yes','no'};
variable(alarm).name='alarm'; variable(alarm).domain = {'yes','no'};
variable(radio).name='radio'; variable(radio).domain = {'yes','no'};

pot(burglar).variables=burglar;
pot(burglar).table(yes)=0.01;
pot(burglar).table(no)=0.99;

pot(earthquake).variables=earthquake;
pot(earthquake).table(yes)=0.000001;
pot(earthquake).table(no)= 1 - pot(earthquake).table(yes);

pot(radio).variables=[radio earthquake];
pot(radio).table(yes,yes)=1;
pot(radio).table(no,yes)=0;
pot(radio).table(yes,no)=0;
pot(radio).table(no,no)=1;

pot(alarm).variables=[alarm burglar earthquake]; % define array below using this variable order
tmptable(yes,yes,yes) = 0.9999;  
tmptable(yes,yes,no) = 0.99;  
tmptable(yes,no,yes) = 0.99;  
tmptable(yes,no,no) = 0.0001;  
tmptable(no,:,:)=1-tmptable(yes,:,:); % normalisation
pot(alarm).table=tmptable;

% do inference:
jointpot = multpots(pot([burglar earthquake alarm radio])); % joint distribution

drawNet(dag(pot),variable);
disp('p(burglar|alarm=yes):')
disptable(condpot(setpot(jointpot,alarm,yes),burglar),variable);

disp('p(burglar|alarm=yes,radio=yes):')
disptable(condpot(setpot(jointpot,[alarm radio],[yes yes]),burglar),variable);
