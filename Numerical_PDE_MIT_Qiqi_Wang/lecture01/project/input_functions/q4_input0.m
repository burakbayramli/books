function [dt, nstep, M, F, ppinitial, qpinitial, u0, c0, H, endt] = q4_input0(M,N)
    % original set of problem parameters
    if nargin < 2
        M = 50;
        N = 200;
    end
    dt = 0.005;
    nstep = 401;
    u0 = sqrt(2);
    c0 = 1;
    L = 6;
    H = 2;
    F = zeros(N/4+1,1);
    endt = (nstep-1) * dt;
    try
        x = find_x(M,N);
    catch
        x = linspace(0,L,N+1);
        y = linspace(0,H,M+1);
        x = repmat(reshape(x,N+1,1),1,M+1) + repmat(reshape(y,1,M+1),N+1,1);
    end
    
%---------------------- only for wrong geometry
%     x = linspace(0,L,N+1);
%     y = linspace(0,H/sqrt(2),M+1);
%     x = repmat(reshape(x,N+1,1),1,M+1) + repmat(reshape(y,1,M+1),N+1,1);
%-------------------------------------------------------------------------
    % get initial pp and qp
    ppinitial = zeros(N+1, M+1);
    qpinitial = zeros(N+1, M+1);
    for i = 1: N+1
        for j = 1:M+1
            if x(i,j) >=2 && x(i,j) <=4 
                ppinitial(i,j) = 0.0001* (1-cos(pi * x(i,j)))^2;
                qpinitial(i,j) = 0.0001 * 2 * pi* (1-cos(pi * x(i,j))) * sin(pi * x(i,j));
            end
        end
    end
end