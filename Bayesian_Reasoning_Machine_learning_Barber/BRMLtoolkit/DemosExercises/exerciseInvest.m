function exerciseInvest
pars.WealthValue=0:0.1:5; % these are the possible states that wealth can take
%numwealthstates=length(WealthValue);

pars.epsilonAval=[0 0.01]; % these are the possible states that price A change by 
pars.epsilonBval=[-0.12 0 0.15]; % these are the possible states that price A change by
% for example, if we use the state 2 of epsilonAval, the price for A at the next timestep will
% be (1+0.01) times the current value. Similarly, if we use state 1 for B,
% B's new price will be (1-0.12) times the current price.

pars.DecisionValue=[1:-0.25:0]; % these are the decision values (order this way so that for two decisions with equally optimal expected utility, the more conservative (safer asset) will be preferred.

% transition matrices:
% asset A
pars.epsilonAtran=condp(ones(length(pars.epsilonAval)));

% asset B
for st=1:length(pars.epsilonBval)
    for stp=1:length(pars.epsilonBval)
        epsilonBtran(stp,st)=exp(- 10*(pars.epsilonBval(stp)-pars.epsilonBval(st)).^2);
    end
end
pars.epsilonBtran=condp(epsilonBtran);

epsilonA(1)=1; epsilonB(1)=1; w(1)=1; % initial states

T=40;
desired=1.5;
[dec val]=optdec(epsilonA(1),epsilonB(1),desired,T,w(1),pars);
fprintf('optimally place %f of wealth in asset A, giving expected wealth at time %d of %f\n',pars.DecisionValue(dec),T,val)