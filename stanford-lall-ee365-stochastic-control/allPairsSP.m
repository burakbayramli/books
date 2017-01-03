function S=allPairsSP(A)
% Floyd-Warshal Algorithm to compute all pairs shortest paths.
S=A;
S(S<10^-10)=10^10;
n=size(A,1);
S=S-diag(diag(S));
for k=1:n   
    for i=1:n
        for j=i+1:n
            if (A(i,j)<10^-10)%Compute only distances that werent available.
                if S(i,k)+S(k,j)<S(i,j)
                    S(i,j)=S(i,k)+S(k,j);
                    S(j,i)=S(i,j);
                end
            end
        end
    end
end