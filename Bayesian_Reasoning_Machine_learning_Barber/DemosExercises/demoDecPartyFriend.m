%DEMODECPARTYFRIEND demo of party-friend decision problem
[party rain friend visit] = assign(1:4); % Variable order is arbitary

yes=1; no=2; in=1; out=2; % define states.

varinf(party).name='party'; varinf(party).domain={'yes','no'};
varinf(rain).name='rain'; varinf(rain).domain={'yes','no'};
varinf(friend).name='friend'; varinf(friend).domain={'in','out'};
varinf(visit).name='visit'; varinf(visit).domain={'yes','no'};

%partial ordering: 
partialorder{1}.max=party;
partialorder{2}.sum=rain;
partialorder{3}.max=visit;
partialorder{4}.sum=friend;

pot(rain).variables=rain; 
pot(rain).table(yes)=0.6; pot(rain).table(no)=0.4; 

pot(friend).variables=[friend rain];
tmptable(in,yes)=0.8; tmptable(out,yes)=0.2;
tmptable(in,no)=0.1; tmptable(out,no)=0.9;
pot(friend).table=tmptable;

% define utilties; 
U(party).variables=[party rain];
U(party).table(yes, yes)=-100;
U(party).table(yes,  no) = 500;
U(party).table(no, yes) =0;
U(party).table(no, no)  = 50;

U(visit).variables=[friend visit party];
U(visit).table=zeros(2,2,2);
U(visit).table(in, yes, no)=200;
U(visit).table(out, yes,  no) = -100;

U(visit).name='visit';
U(party).name='party';

disp('The utilities on the leaf nodes are:')
disptable(sumpots(U([visit party])),varinf,[friend visit rain party]);

% Solve Influence Diagram using Junction Tree:
[jtprob jtutil infostruct]=jtreeID(pot,U,partialorder); % get the Junction Tree
[jtprob jtutil]=absorptionID(jtprob,jtutil,infostruct,partialorder); % do absorption

disp(['Junction Tree: Expected Utilities at the root node:']); disptable(jtutil(end),varinf);


figure(1); drawID(pot,U,partialorder,varinf);title('Influence Diagram')
figure(2); drawJTree(jtutil,infostruct,varinf);title('Strong Junction Tree')
