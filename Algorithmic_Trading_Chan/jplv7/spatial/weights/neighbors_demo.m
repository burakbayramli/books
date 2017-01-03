% demo of finding nearest neighbors

clear all;


load election.dat;
%   col 1= binary y with 0=Dole, 1=Clinton (1996 Presidential Election)
%   col 2 = latt  coordinate
%   col 3 = long  coordinate
%   col 11 = statecode (a number 1 to 48 with the state in which the county is located)

y = election(:,1);  
xc = election(:,2);
yc = election(:,3);

% find 30 nearest neighbors
tic;
nnindex = find_neighbors(xc,yc,30);
toc;

tic;
nnindex2 = find_nn(xc,yc,30);
toc;

% note that the Delauney algorithm can't find more than about 20 neighbors

order = 4;
[j,W,j] = xy2cont(xc,yc);
tic;
nnindex3 = fneighbors2(W,xc,yc,30,order);
toc;

% extract the 30 nearest neighbors to observation/county 20
% covington Alabama

% nnindex takes a form such that: 
%         ind = nnindex(i,:)';
%         y(ind,1) would pull out the m nearest neighbor observations to
%         y(i,1), and y(ind,1)/m would represent an avg of these
ind = nnindex(20,:)';
xc20 = xc(ind,1);
yc20 = yc(ind,1);

xc20a = [xc(20,1)
        xc20];
yc20a = [yc(20,1)
        yc20];
    
ind2 = nnindex3(20,:)';
xc20 = xc(ind2,1);
yc20 = yc(ind2,1);

xc20b = [xc(20,1)
        xc20];
yc20b = [yc(20,1)
        yc20];

% plot these guys
plot(yc,xc,'.');
hold on;
plot(yc20a,xc20a,'.r');
plot(yc20b,xc20b,'.g');
pause;
hold off;

% just do the 30 neighbors plus covington
plot(yc20a,xc20a,'.r');
hold on;
plot(yc20b,xc20b,'og');


