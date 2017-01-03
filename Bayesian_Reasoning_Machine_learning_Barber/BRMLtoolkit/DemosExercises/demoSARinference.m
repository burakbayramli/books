function demoSARinference
%DEMOSARINFERENCE demo of inference in a Switching Autoregressive Model
figure
S = 4; % number of Hidden states
L = 5; % order of each AR model
T = 50; % length of the time-series
Tskip=1; % number of timepoints to skip before state transition is allowed

a = randn(L,S); % set the AR coefficients
for s=1:S; a(:,s) = a(:,s)./sqrt(a(:,s)'*a(:,s)); end % make unit length
stran = condp(rand(S,S)+2*eye(S)); % switch transition
sprior=condp(ones(S,1)); % switch prior
sigma2=0.01*ones(1,S);

% generate some training data:
v=randn(1,T); % random initial visible variable
s(1)=randgen(sprior); % random initial switch state
for t=2:T
    Lt = min(t-1,L); % to handle the start when not enough timepoints exist
    vhat = v(t-Lt:t-1)';
    s(t) = randgen(stran(:,s(t-1)));
    v(t)=a(1:Lt,s(t))'*vhat+sqrt(sigma2(s(t)))*randn;
end
st=zeros(S,T);for t=1:T; st(s(t),t)=1; end % (for plotting later)

% Inference using HMM structure:
[logalpha,loglik]=HMMforwardSAR(v,stran,sprior,a,sigma2,Tskip);
logbeta=HMMbackwardSAR(v,stran,a,sigma2,Tskip);
[phtgV1T,phthtpgV1T]=HMMsmoothSAR(logalpha,logbeta,a,sigma2,stran,v,Tskip);

% Factor graph approach (as a numerical verification):
ht=1:T; % assign numbers to variables
vpot.variables=ht(1);
vpot.table = exp(-0.5*repmat(v(1).^2,S,1)./sigma2(:))./sqrt(2*pi*sigma2(:));
pot(ht(1)).variables=ht(1); pot(ht(1)).table=sprior;
pot(ht(1))=multpots([pot(ht(1)) vpot]);
for t=2:T
    Lt = min(t-1,L); % to handle the start when not enough timepoints
    vhat = v(t-Lt:t-1)';
    m = a(L-Lt+1:L,:)'*vhat; % means
    d = repmat(v(t),S,1)-m;
    vpot.table=exp(-0.5*d.^2./sigma2(:))./sqrt(2*pi*sigma2(:))+eps;
    vpot.variables=ht(t); 
    pot(ht(t)).variables=[ht(t) ht(t-1)];
    if mod(t,Tskip)==0
        strant=stran;
    else
        strant=eye(S);
    end
    pot(ht(t)).table=strant;
    pot(ht(t))=multpots([pot(ht(t)) vpot]); % factor is product of transition and emission
end
AFG = FactorGraph(pot);
[marg mess]=sumprodFG(pot,AFG,[]);
for t=1:T; 	phtgV1Tfg(:,t)=marg(t).table;end

[dum1 fact2var]=FactorConnectingVariable(ht(1),AFG); % can choose any of the variable nodes
tmpmess = multpots(mess(fact2var));
FGloglik = log(sum(tmpmess.table));  % not computed in a numerically stable way

subplot(5,1,1); imagesc(condexp(logalpha)); title('filtered posterior');
subplot(5,1,2); imagesc(phtgV1T); title('smoothed posterior');
subplot(5,1,3); imagesc(phtgV1Tfg); title('smoothed posterior (Factor Graph)');
subplot(5,1,4); imagesc(st); title('sample switches')
colormap bone; subplot(5,1,5); plot(v);
fprintf(1,'mean deviation between Factor Graph and HMM smoothed posterior=%g\n',mean(abs(phtgV1Tfg(:)-phtgV1T(:))));

for t=2:T
    [f fact2var var2fact] = FactorConnectingVariable([ht(t-1) ht(t)],AFG);
    twotimepot = multpots([pot(f) mess(var2fact)]);
    phtmhtV1TFG(:,:,t) = normp(twotimepot.table);
end
fprintf(1,'deviation in the smoothed two time posterior using alpha-beta and Factor Graph is %g\n',...
    mean(mean(mean(abs(phthtpgV1T(:,:,1:end)-phtmhtV1TFG(:,:,2:end))))));

fprintf(1,'Log likelihood from alpha recursion=%g\n',loglik);
fprintf(1,'Log likelihood from Factor Graph=%g\n',FGloglik);