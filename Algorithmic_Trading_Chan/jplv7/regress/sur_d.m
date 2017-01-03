% PURPOSE: An example using sur(),
%                           
% seemingly unrelated regression estimation
% with and without iteration on the error var-cov matrix                              
%---------------------------------------------------
% USAGE: sur_d
%---------------------------------------------------

clear all;

load grun.dat; % grunfeld investment data
                % see page 650, Green 1997

y1  = grun(:,1); % general electric
x11 = grun(:,2);
x12 = grun(:,3);

y2  = grun(:,4); % westinghouse
x21 = grun(:,5);
x22 = grun(:,6);

y3  = grun(:,7); % general motors
x31 = grun(:,8);
x32 = grun(:,9);

y4  = grun(:,10); % chrysler
x41 = grun(:,11);
x42 = grun(:,12);

y5  = grun(:,13); % us steel
x51 = grun(:,14);
x52 = grun(:,15);

nobs = length(y1);
iota = ones(nobs,1);

vname1 = ['I gen motors  ',
          'constant      ',
          'fgm           '
          'cgm           '];
          
vname2 = ['I chrysler    ',
          'constant      ',
          'fcry          '
          'ccry          '];

vname3 = ['I gen electric',
          'constant      ',          
          'fge           ',
          'cge           '];
          
vname4 = ['I westinghouse',
          'constant      ',
          'fwest         '
          'cwest         '];
          
vname5 = ['I us steel    ',
          'constant      ',
          'fuss          '
          'cuss          '];
          
% set up a structure for y containing y's for each eqn
% (order follows that in Green, 1997)
y(1).eq = y3; % gm
y(2).eq = y4; % chrysler
y(3).eq = y1; % general electric
y(4).eq = y2; % westinghouse
y(5).eq = y5; % us usteel

% set up a structure for X in each eqn
X(1).eq = [iota x31 x32];
X(2).eq = [iota x41 x42];
X(3).eq = [iota x11 x12];
X(4).eq = [iota x21 x22];
X(5).eq = [iota x51 x52];

% do sur regression with iteration
neqs = 5;
iflag = 1;
result = sur(neqs,y,X,iflag);

% no iteration done here
result2 = sur(neqs,y,X);


vname = [vname1
         vname2
         vname3
         vname4
         vname5];

prt(result,vname);
prt(result2,vname);


