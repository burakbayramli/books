function[Pos,amb,goodm,goodr,P1goodm,iprnm,iprnr,ref_ind] = goad(no_i)
%GOAD        Ambiguity estimation according to C. Goad's method,
%            see pages 489--490.

% Written by Kai Borre
% November 14, 2007

global EPH

% Initial computations of constants
v_light = 299792458; 	     % vacuum speed of light m/s
f1 = 154*10.23E6;		     % L1 frequency Hz
f2 = 120*10.23E6;		     % L2 frequency Hz
lambda1 = v_light/f1;	     % wavelength on L1:  .19029367  m
lambda2 = v_light/f2;	     % wavelength on L2:  .244210213 m
alpha = (f1/f2)^2;

% Definition of coefficient matrix for ambiguity estimation
A = [1  1 0        0;
    1  -1 lambda1  0;
    1   alpha  0   0;
    1  -alpha  0  lambda2];
sd = [0.3 0.005 0.3 0.005]; % standard deviation of observations
Weight = diag(1./sd.^2);    % diagonal weight matrix
AC = A'*Weight;
% Normals for ambiguity estimation
N = AC*A;
Pos = zeros(4,no_i);

for i = 1:no_i
    [towm,prnm,P1m,Phi1m,P2m,Phi2m] = readGrilM; % fidm
    [towr,prnr,P1r,Phi1r,P2r,Phi2r] = readGrilR; % fidr
    % Find known ephemerides
    prnE = find(EPH(1,:) > 0);
    % Find number lgm of master PRNs with known ephemerides
    [goodm, iprnm] = intersect(prnm,prnE);
    lgm = length(goodm);
    % Find number lgr of rover PRNs with known ephemerides
    [goodr, iprnr] = intersect(prnr,prnE);
    % Note that the above operations leave the PRNs sorted
    % in increasing order

    % Compute elevation angle el in order to deleting low satellites
    [pos, el] = recposRTK(towm,prnm,P1m');
    Pos(:,i) = pos;

    % Delete PRNs with elevation below 10 degrees
    prnm(el < 10) = [];
    % Number of deleted common PRNs due to low elevation
    No_delPRN = lgm-length(prnm);
    if i == 1
        disp(['Number of PRNs deleted due to low elevation ' num2str(No_delPRN)])
    end
    lgm = length(goodm);
    % The PRN with largest elevation is selected as reference
    [y,ref_ind] = max(el);
    % ref_ind keeps the index of the reference PRN and the
    % rest are in sat_ind
    sat_ind = setdiff(1:lgm,ref_ind);
    if lgm >= 4,
        P1goodm = [];
        Phi1goodm = [];
        P2goodm = [];
        Phi2goodm = [];
        for m = 1:lgm
            % rearranging all useful master observations
            P1goodm = [P1goodm; P1m(iprnm(m))];
            Phi1goodm = [Phi1goodm; Phi1m(iprnm(m))];
            P2goodm = [P2goodm; P2m(iprnm(m))];
            Phi2goodm = [Phi2goodm; Phi2m(iprnm(m))];
        end
        P1goodr = [];
        Phi1goodr = [];
        P2goodr = [];
        Phi2goodr = [];
        for m = 1:lgm
            % rearranging all useful rover observations
            P1goodr = [P1goodr; P1r(iprnr(m))];
            Phi1goodr = [Phi1goodr; Phi1r(iprnr(m))];
            P2goodr = [P2goodr; P2r(iprnr(m))];
            Phi2goodr = [Phi2goodr; Phi2r(iprnr(m))];
        end
    end % lgm

    % We loop over all good PRNs (sat_ind).
    % The outer i-loop runs over the epochs from which we
    % want to estimate the ambiguities; the right sides are
    % added and averaged. This works because
    % of the additive character of normal equations
    ddacc = zeros(4,length(sat_ind));
    j = 0;
    for k = sat_ind
        j = j+1;
        dd = [P1goodm(ref_ind)-P1goodm(k)-...
            P1goodr(ref_ind)+P1goodr(k), ...
            Phi1goodm(ref_ind)-Phi1goodm(k)-...
            Phi1goodr(ref_ind)+Phi1goodr(k), ...
            P2goodm(ref_ind)-P2goodm(k)-...
            P2goodr(ref_ind)+P2goodr(k), ...
            Phi2goodm(ref_ind)-Phi2goodm(k)-...
            Phi2goodr(ref_ind)+Phi2goodr(k)];
        ddacc(:,j) = ddacc(:,j)+dd';
    end; %k
end % i

for j = 1:size(ddacc,2)
    x = inv(N)*(AC*(ddacc(:,j)/no_i));
    K1 = round(x(3)-x(4));
    K2 = round(60*x(3)-77*x(4));
    trueN2 = round((60*K1-K2)/17); % (15.10)
    trueN1 = round(trueN2+K1);     % (15.11)
    amb(j,:) = [trueN1 trueN2];
end

% The zero ambiguities of the ref PRN are added
amb(ref_ind,:) = [0 0];
%%%%%%%%%%%%%%%% end goad.m  %%%%%%%%%%%%%%%%%%%%%%%