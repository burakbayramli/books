function moorejs_HW10
% moorejs_HW10.m
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
%
% This function finds the kinetic parameters of a reactor-separator given
% experimental data
%
% input variables: none
% output variables: none

Nstates = 3; % number of conditions tested
Nrepeats = 10; % number of measurements at each condition

% Known parameters (estimates)
kf = 0.42; % forward reaction rate (L/(mol.s))
kr = 0.042; % reverse reaction rate (1/s)
HC = 0.0142; % Henry's law constant for species C (bar)
HA = 0.942; % Henry's law constant for species A (bar)
HB = 0.542; % Henry's law constant for species B (bar)

% Read data from Excel
numx = xlsread('HW8_data.xls');
numx=numx([1:10,12:21,23:end],:);
nrec = numx(:,1); % molar flow of recycle stream (mol/s)
nB0 = numx(:,2); % molar flow rate of B in feed (mol/s)
xAout = numx(:,3); % mole fraction of A in output
xBout = numx(:,4); % mole fraction of B in output
xCout = numx(:,5); % mole fraction of C in output
xArec = numx(:,6); % mole fraction of A in recycle stream
xAreac = numx(:,7); % mole fraction of A in reactor stream
xBrec = numx(:,8); % mole fraction of B in recycle stream

yi = [xAout,xBout,xCout,xArec,xBrec,xAreac];
ni = [nrec,nB0];

% Calculate average measured values and standard deviations
for ns = 1:Nstates
    yave(ns,:) = mean(yi((1:Nrepeats)+Nrepeats*(ns-1),:));
    sigma(ns,:) = std(yi((1:Nrepeats)+Nrepeats*(ns-1),:))/sqrt(Nrepeats);
    n(ns,:) = mean(ni((1:Nrepeats)+Nrepeats*(ns-1),:));
end

% Vary parameters to minimize chi^2
x = [kf,kr,HC,HA,HB];
warning('off');
[x, chi2best] = fmincon(@(x)CalcChi2(yave, n, sigma, x), x,[],[],[],[],...
    [0,0,0,0.9*HA,0.9*HB],[100*kf,10*kr,10*HC,1.1*HA,1.1*HB]);
disp(['Parameters:'])
disp(['kf = ', num2str(x(1)),' L/(mol-s) and kr = ', num2str(x(2)),' s^-1'])
disp(['HA = ',num2str(x(4)),' bar, HB = ',num2str(x(5)),' bar, and HC = ',...
    num2str(x(3)),' bar.'])
disp(['Chi^2 = ',num2str(chi2best),'.'])

Prob = chi2cdf(chi2best,13);
disp(['There is an ',num2str(Prob*100),'% chance that the model is consistent.'])

% Calculate log normalized sensitivities using finite differences
for i = 1:length(x)
    xp = x;
    xp(i) = x(i)*1.01;
    chi2p = CalcChi2(yave, n, sigma, xp);
    xm = x;
    xm(i) = x(i)*0.99;
    chi2m = CalcChi2(yave, n, sigma, xm);
    sens(i) = (log(chi2p)-log(chi2m))/(log(xp(i))-log(xm(i)));
end

disp(['Sensitivities at 1% Change:'])
disp(['kf = ', num2str(sens(1)),' and kr = ', num2str(sens(2))])
disp(['HA = ',num2str(sens(4)),', HB = ',num2str(sens(5)),', and HC = ',...
    num2str(sens(3)),'.'])

% Calculate log normalized sensitivities using finite differences
for i = 1:length(x)
    xp = x;
    xp(i) = x(i)*1.1;
    chi2p = CalcChi2(yave, n, sigma, xp);
    xm = x;
    xm(i) = x(i)*0.9;
    chi2m = CalcChi2(yave, n, sigma, xm);
    sens(i) = (log(chi2p)-log(chi2m))/(log(xp(i))-log(xm(i)));
end

disp(['Sensitivities at 10% Change:'])
disp(['kf = ', num2str(sens(1)),' and kr = ', num2str(sens(2))])
disp(['HA = ',num2str(sens(4)),', HB = ',num2str(sens(5)),', and HC = ',...
    num2str(sens(3)),'.'])
 
% Build chi^2 approximate Jacobian
J = y_model_Jac(n, x);
for i=1:5
    Js(1:6,i) = J(1:6,i)./sigma(1,:)';
    Js(7:12,i) = J(7:12,i)./sigma(2,:)';
    Js(13:18,i) = J(13:18,i)./sigma(3,:)';
end
H = 2*Js'*Js;

kf_vec = linspace(x(1)*0.95,x(1)*1.05,20);
kr_vec = linspace(x(2)*0.8,x(2)*1.2,10);
x_adj = x;

for i = 1:length(kf_vec)
    for j = 1:length(kr_vec)
        
        %Set parameters
        x_adj(1) = kf_vec(i); %kf (L/mol-s)
        x_adj(2) = kr_vec(j); %kr (1/s)
        
        %Compute chi^2 approximation
        chi2_approx(i,j) = chi2best + (x_adj - x)*H*(x_adj - x)';
        
        %Compute chi^2
        chi2(i,j) = CalcChi2(yave, n, sigma, x_adj);
        
        %Generate matrices for contour plot
        kf_Mat(i,j) = kf_vec(i);
        kr_Mat(i,j) = kr_vec(j);
    end
end

% Plot chi^2 dependence on kf and kr
figure(1)
contourf(kf_Mat, kr_Mat, chi2, 20)
colorbar
title('\chi^2')
xlabel('k_f')
ylabel('k_r')

% Plot quadratic approximation to chi^2 dependence on kf and kr
figure(2)
contourf(kf_Mat, kr_Mat, chi2_approx, 20)
colorbar
title('\chi^2 Approximate')
xlabel('k_f')
ylabel('k_r')

% Plot quadratic approximation to chi^2 dependence on ln(kf) and ln(kr)
figure(3)
contourf(log(kf_Mat), log(kr_Mat), chi2_approx, 20)
colorbar
title('\chi^2 Approximate')
xlabel('ln(k_f)')
ylabel('ln(k_r)')

% Calculate the value of chi^2 at 2 degrees of freedom for a 95%
% confidence level
deltachi2 = chi2inv(0.95,2);
chi2max = chi2best + deltachi2;

options = optimset('Display','off');
kfmin = fsolve(@(k)Chi2CI(k, 1, yave, n, sigma, x, chi2max),x(1)*0.95,options);
kfmax = fsolve(@(k)Chi2CI(k, 1, yave, n, sigma, x, chi2max),x(1)*1.05,options);
krmin = fsolve(@(k)Chi2CI(k, 2, yave, n, sigma, x, chi2max),x(2)*0.95,options);
krmax = fsolve(@(k)Chi2CI(k, 2, yave, n, sigma, x, chi2max),x(2)*1.05,options);
kjmin = fsolve(@(k)Chi2CI(k, 1:2, yave, n, sigma, x, chi2max),x(1:2)*0.95,options);
kjmax = fsolve(@(k)Chi2CI(k, 1:2, yave, n, sigma, x, chi2max),x(1:2)*1.05,options);

disp('Individual Confidence Intervals:')
disp(['k_f Confidence Interval = [',num2str(kfmin),', ',num2str(kfmax),']'])
disp(['k_r Confidence Interval = [',num2str(krmin),', ',num2str(krmax),']'])

disp('Joint Confidence Intervals:')
disp(['k_f Confidence Interval = [',num2str(kjmin(1)),', ',num2str(kjmax(1)),']'])
disp(['k_r Confidence Interval = [',num2str(kjmin(2)),', ',num2str(kjmax(2)),']'])

% Create outline of joint confidence interval
for i = 1:10
    for j = 1:10
        kj(10*(i-1)+j,:) = fsolve(@(k)Chi2CI(k, 1:2, yave, n, sigma, x, chi2max),[x(1)*(0.7+i*0.06),x(2)*(j*0.2)],options);
    end 
end

figure(4)
plot(kj(:,1),kj(:,2),'.')
title('Joint Confidence Interval')
xlabel('k_f')
ylabel('k_r')

% sample run
% >> moorejs_HW10
% Parameters:
% kf = 0.34552 L/(mol-s) and kr = 0.074405 s^-1
% HA = 1.0086 bar, HB = 0.54181 bar, and HC = 0.031312 bar.
% Chi^2 = 18.9244.
% There is an 87.4532% chance that the model is consistent.
% Sensitivities:
% kf = -0.30848 and kr = 4.0132e-005
% HA = -0.065093, HB = -0.014462, and HC = -0.00024006.
% Individual Confidence Intervals:
% k_f Confidence Interval = [0.3399, 0.35143]
% k_r Confidence Interval = [0.068489, 0.080241]
% Joint Confidence Intervals:
% k_f Confidence Interval = [0.33251, 0.35897]
% k_r Confidence Interval = [0.066681, 0.081999]

%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function chi2 = Chi2CI(k, element, yave, n, sigma, x, chi2max)
% calculates delta chi^2 for use in parameter confidence interval calculations
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
% input variables:
%   k - reaction parameters to be adjusted
%   element - index of x to be replaced by k
%   yave - average value of experimental mole fractions
%   n - molar flow rates set in each experiment
%   sigma - standard deviation in experimental mole fractions
%   x - reaction parameters [kf (L/(mol-s)), kr (1/s), HC (bar), HA (bar), HB (bar)]
%   chi2max - maximum value of chi^2 allowed for confidence interval
% output variable:
%   chi2 - value of chi^2

x(element) = k;
ycalc = reactorsolver(n, x);

for i = 1:3
    %     chi2 = sum(((yave - ycalc)./sigma).^2);
    chi2(i) = sum(((yave(i,:) - ycalc((1:6)+6*(i-1)))./sigma(i,:)).^2);
end

chi2 = sum(chi2)-chi2max;

%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function chi2 = CalcChi2(yave, n, sigma, x)
% calculates chi^2
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
% input variables:
%   yave - average value of experimental mole fractions
%   n - molar flow rates set in each experiment
%   sigma - standard deviation in experimental mole fractions
%   x - reaction parameters [kf (L/(mol-s)), kr (1/s), HC (bar), HA (bar), HB (bar)]
% output variable:
%   chi2 - value of chi^2

ycalc = reactorsolver(n, x);

for i = 1:3
    %     chi2 = sum(((yave - ycalc)./sigma).^2);
    chi2(i) = sum(((yave(i,:) - ycalc((1:6)+6*(i-1)))./sigma(i,:)).^2);
end

chi2 = sum(chi2);

%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function ycalc = reactorsolver(n, x)
% solves the reactor model equations
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
% input variables:
%   n - molar flow rates set in each experiment
%   x - reaction parameters [kf (L/(mol-s)), kr (1/s), HC (bar), HA (bar), HB (bar)]
% output variable:
%   ycalc - model calculated values of measured mole fractions

options = optimset('Display','off');
for i = 1:3
    ymodel = fsolve(@(ymodel)reactor(ymodel, n(i,:), x), [0,0,1,1,1,1,1,1,1],options);
    
    % Convert molar flow rate into mole fractions and select only those
    % that were measured
    ycalc((1:6)+6*(i-1)) = [ymodel(1:3)/sum(ymodel(1:3)),ymodel(4:5)/sum(ymodel(4:6)),ymodel(7)/sum(ymodel(7:9))];
end


%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function error = reactor(ni, knobs, x)
% calculates the error in the molar flows at steady state in a
% reactor-separator
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
% input variables:
%   ni - molar flows of A, B, and C in the reactor effluent, product stream,
% and recycle stream (mol/s)
%   knobs - molar flow rates set in each experiment [nrec, nB0] (mol/s)
%   x - reaction parameters [kf (L/(mol-s)), kr (1/s), HC (bar)]
% output variable:
%   error - error in the mole fractions

% Known parameters (exact)
nA0 = 1.23; % molar flow rate of A in feed (mol/s)
nC0 = 0; % molar flow rate of C in feed (mol/s)
mwA = 102.34; % molecular weight of A (g/mol)
mwB = 124.08; % molecular weight of B (g/mol)
mwC = 226.42; % molecular weight of C (g/mol)
rhoA = 0.7995; % density of A (kg/L)
rhoB = 1.241; % density of B (kg/L)
rhoC = 1.132; % density of C (kg/L)
PA = 0.52; % saturation pressure of A (bar)
PB = 0.34; % saturation pressure of B (bar)
PC = 0.012; % saturation pressure of C (bar)
Vr = 1.53; % reactor volume (L)

n0 = [nA0, knobs(2), nC0]; % system inlet (mol/s)
kf = x(1); % forward reaction rate constant (L/(mol-s))
kr = x(2); % reverse reaction rate constant (1/s)
Vm = [mwA/1000/rhoA, mwB/1000/rhoB, mwC/1000/rhoC]; % liquid molar volumes (L/mol)
P0 = [PA, PB, PC]; % saturation pressure (bar)
H = [x(4), x(5), x(3)]; % Henry's law constant (bar)
Nrec = knobs(1); % recycle stream molar flow rate (mol/s)

nout = ni(1:3); % molar flow of product stream out separator (mol/s)
nrec = ni(4:6); % molar flow of recycle stream (mol/s)
nreac = ni(7:9); % molar flow out of reactor (mol/s)

Vflow = nreac*Vm'; % flowrate out of reactor (L/s)
CA = nreac(1)/Vflow; % concentration of A in reactor (mol/L)
CB = nreac(2)/Vflow; % concentration of B in reactor (mol/L)
CC = nreac(3)/Vflow; % concentration of C in reactor (mol/L)
r = kf*CA*CB-kr*CC; % reaction rate (mol/(L-s))

x = nout/sum(nout); % mole fraction in product stream (mol/mol)
p = (P0-H).*x.^2+H.*x; % partial pressure (bar)
Ptot = sum(p); % total pressure of recycle stream (bar)

% balance around reactor
error(1:3) = n0 + p/Ptot*Nrec - nreac - r*Vr*[1,1,-1];

% balance around separator
error(4:6) = nreac - p/Ptot*Nrec - nout;

% recycle stream flow rates
error(7:9) = nrec - p/Ptot*Nrec;

error = error';

%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function J = y_model_Jac(n, x)
%function computes the Jacobian of y_model with respect to Theta. Each
%observable in each experiment is treated as one element of y_model, so
%that the Jacobian is 18-by-5. The Jacobian is computed using the finite
%differencing function below for each experiment to get three 6-by-5 blocks
%of the Jacobian.
% Written by: Jason Moore on Nov. 5, 2007
% Last modified by: Jason Moore on Dec. 3, 2010
% input variables:
%   n - molar flow rates set in each experiment
%   x - the value of the adjusted parameters at which the Jacobian is evaluated
% output variable:
%   J - the 18-by-5 Jacobian

for i = 1:length(x)
    xp = x;
    xp(i) = x(i)*1.01;
    ycalcp = reactorsolver(n, xp)';
    xm = x;
    xm(i) = x(i)*0.99;
    ycalcm = reactorsolver(n, xm)';
    J(:,i) = (ycalcp-ycalcm)/(xp(i)-xm(i));
end