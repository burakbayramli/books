function demoClouseau
%DEMOCLOUSEAU inspector clouseau example
butler=1; maid=2; knife=3; % Variable order is arbitary
murderer=1; notmurderer=2; used=1; notused=2; % define states, starting from 1.

% The following definitions of variable are not necessary for computation,
% but are useful for displaying table entries:
variable(butler).name='butler'; variable(butler).domain = {'murderer','not murderer'};
variable(maid).name='maid'; variable(maid).domain ={'murderer','not murderer'};
variable(knife).name='knife'; variable(knife).domain={'used','not used'};

% Three potentials since p(butler,maid,knife)=p(knife|butler,maid)p(butler)p(maid).
% potential numbering is arbitary
pot(butler).variables=butler;
pot(butler).table(murderer)=0.6;
pot(butler).table(notmurderer)=0.4;

pot(maid).variables=maid;
pot(maid).table(murderer)=0.2;
pot(maid).table(notmurderer)=0.8;

pot(knife).variables=[knife,butler,maid]; % define array below using this variable order
pot(knife).table(used, notmurderer, notmurderer)=0.3;  
pot(knife).table(used, notmurderer, murderer)   =0.2;
pot(knife).table(used, murderer,    notmurderer)=0.6;
pot(knife).table(used, murderer,    murderer)   =0.1;
pot(knife).table(notused,:,:)=1-pot(knife).table(used,:,:); % due to normalisation

jointpot = multpots(pot([butler maid knife])); % joint distribution

drawNet(dag(pot),variable);
disp('p(butler|knife=used):')
disptable(condpot(setpot(jointpot,knife,used),butler),variable);