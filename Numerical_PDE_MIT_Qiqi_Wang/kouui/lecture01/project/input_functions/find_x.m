function x = find_x(M,N)
    F = zeros(N/4+1,1);
    [~, x, y] = q4(0.005, 1, M, F, zeros(N+1,M+1),zeros(N+1,M+1));
    if min(size(x))==1
        x = repmat(reshape(x,N+1,1),1,M+1) + repmat(reshape(y,1,M+1),N+1,1);
    end
    assert(size(x,1) == N+1);
    assert(size(x,2) == M+1);
end