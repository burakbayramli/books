function demoHMMinference
%DEMOHMMINFERENCE demo of inference in a Hidden Markov Model
H = 6; % number of Hidden states
V = 2; % number of Visible states
T = 5; % length of the time-series
% setup the HMM
phghm = rand(H,H); phghm = phghm./repmat(sum(phghm,1),H,1);% transition distribution p(h(t)|h(t-1))
pvgh = rand(V,H); pvgh = pvgh./repmat(sum(pvgh,1),V,1);% emission distribution p(v(t)|h(t))
ph1 = rand(H,1); ph1=ph1./sum(ph1); % initial p(h)
% generate some fake data
h(1) = randgen(ph1); v(1)=randgen(pvgh(:,h(1)));
for t=2:T
	h(t)=randgen(phghm(:,h(t-1)));	v(t)=randgen(pvgh(:,h(t)));
end
% Perform Inference tasks:
[logalpha,loglik]=HMMforward(v,phghm,ph1,pvgh); % forward
logbeta=HMMbackward(v,phghm,pvgh); % backward
% smoothed posteriors:
[phtgV1T,phthtpgV1T]=HMMsmooth(logalpha,logbeta,pvgh,phghm,v);
gamma=HMMgamma(logalpha,phghm); % alternative alpha-gamma (RTS) method
[viterbimaxstate logprob]=HMMviterbi(v,phghm,ph1,pvgh); % most likely joint state

% Factor graph approach: (Note the FG code is not programmed to avoid underflow)
ht=1:T; vt=T+1:2*T; % assign numbers to variables
%domain(ht)=blankstates(H); domain(vt)=blankstates(V);
pot(ht(1)).variables=ht(1); pot(ht(1)).table=ph1;
pot(vt(1)).variables=[vt(1) ht(1)]; pot(vt(1)).table=pvgh;
for t=2:T
	pot(vt(t)).variables=[vt(t) ht(t)];  pot(vt(t)).table=pvgh;
	pot(ht(t)).variables=[ht(t) ht(t-1)]; pot(ht(t)).table=phghm;
end
%pot=setdomain(pot,domain);
for t=1:T; newpot(t)=multpots([setpot(pot(vt(t)),vt(t),v(t)) pot(ht(t))]); end

AFG = FactorGraph(newpot);
[marg mess]=sumprodFG(newpot,AFG,[]);
% likelihood
[dum1 fact2var]=FactorConnectingVariable(ht(1),AFG); % can choose any of the variable nodes
tmpmess = multpots(mess(fact2var));
FGloglik = log(sum(tmpmess.table));  % not computed in a numerically stable way
for t=1:T
	post(:,t)=evalpot(marg(t));
end

figure; drawFG(AFG)
% two timepoint probabilities: multiply the factor by the incomining variable to factor messages:
for t=2:T
	[f fact2var var2fact] = FactorConnectingVariable([ht(t-1) ht(t)],AFG);
	twotimepot = multpots([newpot(f) mess(var2fact)]);
	phthtmgV1T(:,:,t) = normp(twotimepot.table);
end

fprintf(1,'Likelihood from the logalpha recursion = %g\n',loglik);
fprintf(1,'Likelihood from the Factor Graph= %g\n',FGloglik);
fprintf(1,'deviation in the smoothed posterior using alpha-beta and Factor Graph is %g\n', ...
	mean(mean(abs(phtgV1T- post))));
fprintf(1,'deviation in the smoothed posterior using alpha-gamma and Factor Graph is %g\n', ...
	mean(mean(abs(gamma- post))));
fprintf(1,'deviation in the smoothed two time posterior using alpha-beta and Factor Graph is %g\n',...
mean(mean(mean(abs(phthtpgV1T(:,:,1:end)-phthtmgV1T(:,:,2:end))))));

fprintf(1,'Viterbi: most likely log prob =%g\n and state:\n',logprob);
disp(viterbimaxstate);
[maxstate maxval]=maxprodFG(newpot,AFG,[]);
fprintf(1,'Factor Graph: most likely log prob =%g\n and state:\n',log(maxval));
disp(maxstate);