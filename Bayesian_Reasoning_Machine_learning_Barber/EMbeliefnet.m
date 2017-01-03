function [newpot loglik]=EMbeliefnet(pot,x,pars)
%EMBELIEFNET train a Belief Network using Expectation Maximisation
% [pot loglik]=EMbeliefnet(pot,x,pars)
% EM training of the table entries for a Belief Network
% pot contains the BN, with pot(i) containing p(x_i|pa(x_i)).
% If pot(i).table is empty, it is initialised at random.
%
% The data x must be sorted according to the variables in pot.
% That is x(1,:) contains the data for variable x_1, etc.
% missing entries are indicated with nan
%
% pars.PotentialsToUpdate : vector of potentials to learn. If missing, all tables are updated
% pars.tol : congerence criterion on log likelihood.
% pars.maxiterations : maximum number of EM iterations
% pars.plotprogress : set to 1 to plot the log likelihood during training.
[V N] =size(x);
[variables nstates]=potvariables(pot);
oldpot=pot;
if isfield(pars,'PotentialsToUpdate')
    PotentialsToUpdate=pars.PotentialsToUpdate;
else
    PotentialsToUpdate=variables;
end
tol=0.0001;
if isfield(pars,'tol')
    tol=pars.tol;
end
maxiterations=10;
if isfield(pars,'maxiterations')
    maxiterations=pars.maxiterations;
end
plotprogress=0;
if isfield(pars,'plotprogress')
    plotprogress=pars.plotprogress;
    if plotprogress==1; figure; end;
end
% make a conditional probability p(i|pa(i))
for i=PotentialsToUpdate
    if  all(pot(i).table(:)==0)
        oldpot(i).table=myrand(nstates(pot(i).variables));
        oldpot(i)=condpot(oldpot(i),i,setdiff(pot(i).variables,i));
    end
end

newpot=oldpot; % use to initialise any tables which are not learned.
for emloop=1:maxiterations
    loglik=0;
    % E step:
    for i=PotentialsToUpdate % initialise all the summed potentials to zero
        smpot(i)=oldpot(i);
        smpot(i).table=zeros(size(oldpot(i).table))+0.001; % add a small value for when no data
    end
    
    for n=1:N
        missing = find(isnan(x(:,n)));
        present=setdiff(1:V,missing);
        % get the log likelihood:
        qx=multpots(setpot(oldpot,present,x(present,n))); % prob of visible and missing
        thisloglik=log(evalpot(sumpot(qx,missing))); % slow on large networks (use JT there instead)
        loglik = loglik + thisloglik;
        % compute statistics needed for the M-step:
        for i=PotentialsToUpdate
            ivars=pot(i).variables;
            ipresent=intersect(present,ivars);
            qiv=deltapot(ipresent,x(ipresent,n),nstates(ipresent));
            imissing=intersect(missing,ivars);
            if isempty(imissing)
                smpot(i)=sumpots([smpot(i) qiv]);
            else
                qihgv= condpot(qx,imissing);
                smpot(i)=sumpots([smpot(i) multpots([qihgv qiv])]);
            end
        end
    end
    
    for i=PotentialsToUpdate; newpot(i) = condpot(smpot(i),i,setdiff(oldpot(i).variables,i)); end % M step
    
    logloop(emloop)=loglik;
    if plotprogress;plot(logloop,'-o'); ylabel('log likelihood');drawnow;end
    if emloop>1;
        if logloop(end)-logloop(end-1)<tol; break; end % convergence criterion
    end
    oldpot=newpot;
end