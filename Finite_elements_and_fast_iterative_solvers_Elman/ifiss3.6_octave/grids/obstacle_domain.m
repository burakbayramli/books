function obstacle_domain
%OBSTACLE_DOMAIN obstacle domain Q2 grid generator
%   obstacle_domain;
%
% grid defining data is saved to the file: obstacle_grid.mat
%   IFISS function: HCE; 24 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
% Code written by M. Wu, 2009

fprintf('\n\nGrid generation for domain with obstacle.\n')
nc=default('grid parameter: 3 for underlying 8x20 grid (default is 4)',4);
if nc<3
    error('illegal parameter choice, try again.')
elseif nc == 3
 fprintf('**** Warning: increasing obstacle size for nc=3 ****\n');
end
grid_type=default('uniform/stretched grid (1/2) (default is uniform)',1);

n=2^nc;
obs = [];xy = [];mv = []; sbnde = [];


%% user input (problem specific)

% bndxy: vertices that define the domain and obstacle(s) 
%        (x-coordinate y-coordinate)
% bnde: boundary edges 
%      (node1 node2 1(for dirichlet)/0(for neumann))
% obs: obstacles 
%      (node1 node2 node3 node4)
% sbnde: boundary edges near which stretching is needed 
%       (edge1 edge2 ...)
% 'obs' and/or 'sbnde' can be absent if there is no obstacle in the problem
% and/or only uniform grid is needed
if nc==3
    bndxy = ...
           [ 0   -1.0000
        8.0000   -1.0000
        8.0000    1.0000
             0    1.0000
        1.5000   -0.5000
        2.5000   -0.5000
        2.5000    0.5000
        1.5000    0.5000];
else
    bndxy = ...
           [ 0   -1.0000
        8.0000   -1.0000
        8.0000    1.0000
             0    1.0000
        1.7500   -0.2500
        2.2500   -0.2500
        2.2500    0.2500
        1.7500    0.2500];
end

bnde = ...
       [1     2     1
     2     3     0
     3     4     1
     4     1     1
     5     6     1
     6     7     1
     7     8     1
     8     5     1];
 obs = ...
       [5     6     7     8];
 sbnde = [ 5 6 7 8];

%% compute mesh size h and uniform x, y coordinate (problem nonspecific)

h = min(max(bndxy(:,2)) - min(bndxy(:,2)),max(bndxy(:,1)) - min(bndxy(:,1)))/n;
x = [min(bndxy(:,1)):h:max(bndxy(:,1))]';
y = [min(bndxy(:,2)):h:max(bndxy(:,2))]';

%% generate xy coordinates and element mapping matrix (problem specific)

% [xyi,mvi] = rec_domain_q2(x1,x2,y1,y2,h,xy)

[xy1,mv1] = rec_domain_q2(0,bndxy(obs(1),1),-1,1,h,xy);
xy = [xy;xy1];
mv = [mv;mv1];

[xy2,mv2] = rec_domain_q2(bndxy(obs(1),1),bndxy(obs(2),1),-1,bndxy(obs(1),2),h,xy);
xy = [xy;xy2];
mv = [mv;mv2];

[xy3,mv3] = rec_domain_q2(bndxy(obs(1),1),bndxy(obs(2),1),bndxy(obs(3),2),1,h,xy);
xy = [xy;xy3];
mv = [mv;mv3];

[xy4,mv4] = rec_domain_q2(bndxy(obs(2),1),8,-1,1,h,xy);
xy = [xy;xy4];
mv = [mv;mv4];

%% compute boundary vertices and edges (problem nonspecific)

[bound,mbound] = findboundary(bndxy,bnde,xy,mv);

%% compute stretched grid (problem nonspecific)

if grid_type == 2
     [xs,ys,xys] = stretch_grid(x,y,xy,bndxy,bnde,sbnde,h);
     x = xs; y = ys; xy = xys;
end

%% save grid file (only file name is problem specific)
gohome 
cd datafiles
save obstacle_grid.mat mv xy bound mbound x y ...
        bndxy bnde obs %stretch
%% 
return
