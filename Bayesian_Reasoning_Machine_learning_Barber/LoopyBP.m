function [marg mess A]=LoopyBP(pot,varargin)
%LOOPYBP loopy Belief Propagation using sum-product algorithm
%[marg mess A]=LoopyBP(pot,<opts>)
% options: opts.maxit, opts.tol, opts.plotprogress
opts=[];if nargin==2; opts=varargin{1}; end
opts=setfields(opts,'maxit',5*length(pot),'tol',1e-10,'plotprogress',1);% default options
A = FactorGraph(pot);
nmess = full(sum(A(:)~=0)); % total number of messages
messlidx = find(A); % message indices
del=[];
for loop=1:opts.maxit
    r=randperm(nmess); % random message schedule (leave random spanning tree schedule as an exercise)    
    for i=1:nmess
        A(messlidx(i))=r(i); % place message index on A
        k(r(i))=i; % number the message potentials accordingly
    end
    if loop>1
        [marg mess(k)]=sumprodFG(pot,A,mess(k));
        mess=normpot(mess);
        if mean(abs(vertcat(marg.table)-vertcat(oldmarg.table)))<opts.tol; break; end
        if opts.plotprogress
            del=[del mean(abs(vertcat(marg.table)-vertcat(oldmarg.table)))];
            plot(del,'-o');
        end
    else
        [marg mess]=sumprodFG(pot,A,[]); mess(k)=mess;
    end
    oldmarg=marg;
end
mess=mess(k);