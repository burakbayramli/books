%EASY141 Script for computing SBAS horizontal and vertical
%        protection levels

%Kai Borre 10-07-2008
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 2010/04/18  $

% Output
%   HPL  - Horizontal Protection Level
%   VPL  - Vertical Protection Level

% assigning constants
dtr = pi/180;
K_H = 6.0;
K_V = 5.33;

% Row-wise satellite coordinates and pseudoranges
D = [...
    14177509.188  -18814750.650   12243944.449  21119263.116;
    15097198.146   -4636098.555   21326705.426  22527063.486;
    23460341.997   -9433577.991    8174873.599  23674159.579;
    -8206498.071  -18217989.839   17605227.065  20951643.862;
    1399135.830  -17563786.820   19705534.862  20155386.649;
    6995655.459  -23537808.269   -9927906.485  24222112.972];

% Receiver coordinates
M = [596902.683   -4847843.316    4088216.740];
s = size(D,1);

for i = 1:s
    A(i,:) = [ -(D(i,1)-M(1))/D(i,4) -(D(i,2)-M(2))/D(i,4) -(D(i,3)-M(3))/D(i,4) 1];
end

[phi,lambda,h] = togeod(6378137,298.257223563,M(1),M(2),M(3));
cl = cos(lambda*dtr); sl = sin(lambda*dtr);
cb = cos(phi*dtr); sb = sin(phi*dtr);
F = [ -sl -sb*cl cb*cl;
       cl -sb*sl cb*sl;
       0     cb    sb];

B = zeros(s,3); % B = -A(:,1:3)*F = -(rows of A) * [e n u]
Az = [];
El = [];
for i = 1:s
    [az,el,dis] = topocent(M',(D(i,1:3)-M)');
    az = az*dtr; % az is counted positive clockwise from north
    el = el*dtr;
    Az = [Az az];
    El = [El el]; 
    B(i,1:3) = [cos(el)*sin(az) cos(el)*cos(az) sin(el)];
end
B = [B ones(s,1)]
B_x = [-A(:,1:3)*F ones(s,1)];

%               Note: positive azimuth is defined clockwise from North
%   sigma     - sigma2_flt + sigma2_tropo + sigma2_iono + sigma2_air for all
%               satellites (min 4). From EGNOS

% Now follows the HPL and VPL computation
% Given
% az = [   ]; % unit radian
% el = [   ]; % unit radian
% sigma = 1;  % unit meter

if s <= 4
    disp('Not enough PRNs'), break, end;

sigma = ones(1,s); % need to input expression
W = diag(1./sigma);
Sigma = inv(B'*W*B);
lambda1  = (Sigma(1,1)+Sigma(2,2) + sqrt( (Sigma(1,1)+Sigma(2,2))^2 ...
    -4*(Sigma(1,1)*Sigma(2,2)-(Sigma(1,2))^2) ))/2;
HPL = K_H*sqrt(lambda1)%;
VPL = K_V*sqrt(Sigma(3,3))%;

%%%%%%%%%%%%%%%%%%%%%%%%%%% easy141.m  %%%%%%%%%%%