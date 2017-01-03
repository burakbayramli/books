function [f F cutS cutT]=MaxFlow(w,s,t)
%MaxFlow Ford Fulkerson max flow - min cut algorithm (breadth first search)
% [f F cutS cutT]=MaxFlow(w,s,t)
% w(j,i) is the capacity of edge i->j, s the source and t the sink
N = size(w,1);
R = w; % residual network
f=0;
while true
    augpath=mostprobablepath(replace(real(R>0),[0 1],[-inf -1]),s,t); % find shortest path from s to t
    if isnan(augpath); break; % stop if no augmenting path exists
    else
        for i=1:length(augpath)-1
            l(i)=R(augpath(i+1),augpath(i));
        end
        m = min(l);
        f=f+m; % increase the flow
        for i=1:length(augpath)-1 % update the residual network
            R(augpath(i+1),augpath(i))=R(augpath(i+1),augpath(i))-m;
            R(augpath(i),augpath(i+1))=R(augpath(i),augpath(i+1))+m;
        end
    end
end
cutS = [s ancestors(s,R>0)]; % cut members connected to source
cutT = setdiff(1:N,cutS); % cut members connected to sink
F=w-R; F(F<0)=0; % flow