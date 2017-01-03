function [val]=MDPsolve(tran,util,gam,opts)
%MDPSOLVe solve a Markov Decision Process
% val=MDPsolve(tran,util,gam,opts)
% tran : transition probability matrix
% util : utility matrix
% gam : discounting factor
% opts.method={'value','policy'}
% see also demoMDP
[xt xtm dtm]=assign(1:3); % assign the variables x(t), x(t-1), d(t-1) to some numbers
% define the domains of the variables
S = size(tran,1); A = size(tran,3);

% define the transition potential: p(x(t)|x(t-1),d(t-1))
tranpot.variables=[xt xtm dtm]; tranpot.table=tran;
% setup the value potential: v(x(t))
valpot.variables=xt; valpot.table=ones(S,1); % initial values
oldvalue=valpot.table;

% Value Iteration:
if strcmp(opts.method,'value')
    for valueloop=1:opts.maxiterations
        tmppot = maxpot(sumpot(multpots([tranpot valpot]),xt),dtm);
        valpot.table = util + gam*tmppot.table; % Bellman's recursion
        if mean(abs(valpot.table-oldvalue))<opts.tol; break; end % stop if converged
        oldvalue = valpot.table;
        if opts.plotprogress; 	bar(valpot.table); drawnow; end
    end
end

% Policy Iteration:
if strcmp(opts.method,'policy')
    pdstar=zeros(S,S);
    for policyloop=1:opts.maxiterations
        % Policy evaluation: get the optimal decisions as a function of the state:
        [tmppot dstar] = maxpot(sumpot(multpots([tranpot valpot]),xt),dtm);
        for x1=1:S
            for x2=1:S
                pdstar(x1,x2) = tran(x2,x1,dstar(x1));
            end
        end
        valpot.table = (eye(S)-gam*pdstar)\util;
        if mean(abs(valpot.table-oldvalue))<opts.tol; break; end % stop if converged
        oldvalue=valpot.table;
        if opts.plotprogress; 	bar(valpot.table); drawnow; end
    end
end
val=valpot.table;