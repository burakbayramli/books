function [dt, nstep, M, F, ppinitial, qpinitial, u0, c0, H, endt] = q2_input0(M,N)
    % original set of problem parameters
    if nargin < 2
        M = 50;
        N = 200;
    end
    dt = 0.01;
    nstep = 801;
    u0 = sqrt(2);
    c0 = 1;
    L = 6;
    H = 2;
    F = zeros(N/4+1,1);
    x = linspace(0, L, N+1);
    endt = (nstep-1) * dt;
    % get initial pp and qp
    ppinitial = zeros(N+1, M+1);
    qpinitial = zeros(N+1, M+1);
    for i = 1: N+1
        if x(i) >=0 && x(i) <=2 
        ppinitial(i,:) = 0.0001* (1-cos(pi * x(i)))^2;
        qpinitial(i,:) = 0.0001 * 2 * pi* (1-cos(pi * x(i))) * sin(pi * x(i));
        end
    end
end