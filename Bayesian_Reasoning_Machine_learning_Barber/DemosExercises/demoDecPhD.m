function demoDecPhD
%DEMODECPHD demo of Phd decision problem
disp('PhD scenario:'); 
[education income prize]=assign(1:3); % Variable order is arbitary

varinf(education).name='education'; varinf(education).domain={'do phd','no phd'}; [st_phd st_nophd]=assign(1:2);
varinf(income).name='income'; varinf(income).domain={'low','average', 'high'}; [st_low st_av st_high]=assign(1:3);
varinf(prize).name='prize'; varinf(prize).domain={'prize','no prize'}; [st_prize st_noprize]=assign(1:2);

%partial ordering: partialorder{1}<partialorder{2}<partialorder{3}<...
partialorder{1}.max=education;
partialorder{2}.sum=[income prize];

pot(prize).variables=[prize education];
pot(prize).table(st_prize,st_nophd)=0.0000001; 
pot(prize).table(st_prize,st_phd)=0.001;
pot(prize).table(st_noprize,:)=1-pot(prize).table(st_prize,:); % normalisation

pot(income).variables=[income education prize];
pot(income).table(st_low,st_phd,st_noprize) =0.1;
pot(income).table(st_av,st_phd,st_noprize)  =0.5;
pot(income).table(st_high,st_phd,st_noprize)=0.4;
pot(income).table(st_low,st_nophd,st_noprize) =0.2;
pot(income).table(st_av,st_nophd,st_noprize)  =0.6;
pot(income).table(st_high,st_nophd,st_noprize)=0.2;
pot(income).table(st_low,st_phd,st_prize) =0.01;
pot(income).table(st_av,st_phd,st_prize)  =0.04;
pot(income).table(st_high,st_phd,st_prize)=0.95;
pot(income).table(st_low,st_nophd,st_prize) =0.01;
pot(income).table(st_av,st_nophd,st_prize)  =0.04;
pot(income).table(st_high,st_nophd,st_prize)=0.95;

% define utilties; 
util(1).variables=education;
util(1).table(st_phd)=-50000;
util(1).table(st_nophd)=0;
util(1).name='education';
util(2).variables=income;
util(2).table(st_low) =100000;
util(2).table(st_av)  =200000;
util(2).table(st_high)=500000;
util(2).name='income';

% Influence Diagram solved using Junction Tree:
[jtprob jtutil infostruct]=jtreeID(pot,util,partialorder); % get the Junction Tree
[jtprob jtutil]=absorptionID(jtprob,jtutil,infostruct,partialorder); % do absorption
disptable(jtutil(end),varinf);
figure; title('PhD scenario');
drawID(pot,util,partialorder,varinf)


% Startup scenario: 
disp('Startup scenario:')
[education income prize startup]=assign(1:4); 

varinf(education).name='education'; varinf(education).domain={'do phd','no phd'}; [st_phd st_nophd]=assign(1:2);
varinf(income).name='income'; varinf(income).domain={'low','average', 'high'}; [st_low st_av st_high]=assign(1:3);
varinf(prize).name='prize'; varinf(prize).domain={'prize','no prize'}; [st_prize st_noprize]=assign(1:2);
varinf(startup).name='startup'; varinf(startup).domain={'startup','no startup'};[st_startup st_nostartup]=assign(1:2);

%partial ordering: partialorder{1}<partialorder{2}<partialorder{3}<...
partialorder{1}.max=education;
partialorder{2}.sum=prize;
partialorder{3}.max=startup;
partialorder{4}.sum=income;

pot(income).variables=[income startup prize];
pot(income).table(st_low,st_startup,st_noprize) =0.1;
pot(income).table(st_av,st_startup,st_noprize)  =0.5;
pot(income).table(st_high,st_startup,st_noprize)=0.4;
pot(income).table(st_low,st_nostartup,st_noprize) =0.2;
pot(income).table(st_av,st_nostartup,st_noprize)  =0.6;
pot(income).table(st_high,st_nostartup,st_noprize)=0.2;
pot(income).table(st_low,st_startup,st_prize) =0.005;
pot(income).table(st_av,st_startup,st_prize)  =0.005;
pot(income).table(st_high,st_startup,st_prize)=0.99;
pot(income).table(st_low,st_nostartup,st_prize) =0.05;
pot(income).table(st_av,st_nostartup,st_prize)  =0.15;
pot(income).table(st_high,st_nostartup,st_prize)=0.8;

util(3).variables=startup;
util(3).table(st_startup)  =-200000;
util(3).table(st_nostartup)=0;
util(3).name='startup';

% Influence Diagram solved using Junction Tree:
[jtprob jtutil infostruct]=jtreeID(pot([income prize]),util(1:3),partialorder); % get the Junction Tree
[jtprob jtutil]=absorptionID(jtprob,jtutil,infostruct,partialorder); % do absorption
disptable(jtutil(end),varinf);
figure; title('Startup scenario');
drawID(pot,util,partialorder,varinf)