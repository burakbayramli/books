% Plane Stress Assembly Example
e = 10^4; nu = 0.2; h=0.25; q=-20;
nodes=[0,0; 0,2; 2,0; 2,3/2; 4,0; 4,1];
conn = [1,3,4; 4,2,1; 3,5,6; 6,4,3];
lmm = [1,2,5,6,7,8; 7,8,3,4,1,2; 
    5,6,9,10,11,12; 11,12,7,8,5,6];

K=zeros(12); R = zeros(12,1);
% Generate equations for each element and assemble them.
for i=1:4
    con = conn(i,:);
    lm = lmm(i,:);
    k = PlaneStressTriElement(e, nu, h, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
end
% Define the nodal load vector
con = conn(2,:);
lm = lmm(2,:);
rq = PlaneStressTriLoad(1,q,0,h,nodes(con,:));
R(lm) = R(lm) + rq;

con = conn(4,:);
lm = lmm(4,:);
rq = PlaneStressTriLoad(1,q,0,h,nodes(con,:));
R(lm) = R(lm) + rq;
K
R