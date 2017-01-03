function [q st]=subsetsum(x)
%SUBSETSUM Solve the zero subset sum problem.
% Input: x  - a vector of integers
% Outputs: q=1 if there is a binary 0/1 vector st such that
% sum_i x(i).*st(i) = =0
% The method used is either dynamic programming or explicit enumeration,
% depending on the number of variables and the precision of the integers.

% The wikipedia dynamic programming method has complexity O(n*(P-N)), 
% where n is the number of variables, P the sum of positive x and N the sum of negative x.
% This algorithmn is therefore practical only when the precision (number of
% bits required to specify the elements of x is small. Otherwise, for small
% n, and larger precision, one can just enumerate all 2^n binary states,
% which would be more efficient.
n=length(x);
N=sum(x(x<0));
P=sum(x(x>0));

if n*log(2)> (log(n)+log(P-N)); do_dynamic_programming=1;
else
    do_dynamic_programming=0;
end

if do_dynamic_programming
    
    offset=-min(N-x)+1; % offset ensures no negative indices
    Q=zeros(max(P-x+offset));
    for s=N:P
        Q(1,s+offset)= (x(1)==s);
    end
    
    for i=2:n
        for s=N:P
            Q(i,s+offset)=Q(i-1,s+offset)| (x(i)==s) | Q(i-1,s-x(i)+offset);
        end
    end
    
    q=Q(n,0+offset); % q=1 if there is a solution
    st=zeros(1,n); % initialise solution
    if q % there is a solution, so find by backtracking:
        % idea is to start from the last point x(n). If there is a solution in
        % which the sum of a subset up to and including n-1 equals -x(n), then we
        % can include x(n) in the solution. We then add x(n) to the total, and move
        % on to x(n-1):
        tot=0;
        for i=n:-1:2
            if tot+x(i)==0
                st(i)=1;
                break;
            end
            if Q(i-1,offset-tot-x(i))
                st(i)=1;
                tot=tot+x(i);
                if tot==0; break; end
            end
        end
        if sum(st(1:n).*x(1:n))~=0
            st(1)=1;
        end
    end
else % explicit enumeration of all 2^n states:
    st=ind2subv(2*ones(1,n),1:2^n)-1;
    vals=st*x(:);
    ind=find(vals==0);
    if isempty(ind)
        q=0; st=zeros(1,n);
    else
        q=1; st=vals(ind(1));
    end
end
function out = ind2subv(siz,ndx)
% out = ind2subv(siz,ndx)
%
% index to state : return a state vector of the linear index ndx, based on an array of size siz
% For vector ndx, returns a matrix with each row containing the corresponding state vector
k = [1 cumprod(siz(1:end-1))];
for i = length(siz):-1:1; vi = rem(ndx-1, k(i)) + 1; vj = (ndx - vi)./k(i) + 1;out(:,i) = vj;ndx = vi; end
