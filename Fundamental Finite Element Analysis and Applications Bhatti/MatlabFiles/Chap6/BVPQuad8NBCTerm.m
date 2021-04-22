function [ka, rb] = BVPQuad8NBCTerm(side, alpha, beta, coord)
% [ka, rb] = BVPQuad8NBCTerm(side, alpha, beta, coord)
% Generates kalpha and rbeta when NBC is specified along a side
% side = side over which the NBC is specified
% alpha and beta = coefficients specifying the NBC
% coord = coordinates at the element ends

% Use 3 point integration. Gauss point locations and weights
gpLocs = [-sqrt(3/5), 0, sqrt(3/5)];
gpWts = [5/9, 8/9, 5/9];
ka=zeros(8); rb=zeros(8,1);
for i=1:length(gpWts)
    a = gpLocs(i); w = gpWts(i);
    switch (side)
    case 1
        n = [(1 - a)/2 + (-1 + a^2)/2, 1 - a^2, ...
                (1 + a)/2 + (-1 + a^2)/2, 0, 0, 0, 0, 0];
        dna = [-1/2 + a, -2*a, 1/2 + a, 0, 0, 0, 0, 0];
    case 2
        n = [0, 0, (1 - a)/2 + (-1 + a^2)/2, ...
                1 - a^2, (1 + a)/2 + (-1 + a^2)/2, 0, 0, 0];
        dna = [0, 0, -1/2 + a, -2*a, 1/2 + a, 0, 0, 0];
    case 3
        n = [0, 0, 0, 0, (1 - a)/2 + (-1 + a^2)/2, ...
                1 - a^2,(1 + a)/2 + (-1 + a^2)/2, 0];
        dna = [0, 0, 0, 0, -1/2 + a, -2*a, 1/2 + a, 0];
    case 4
        n = [(1 + a)/2 + (-1 + a^2)/2, 0, 0, 0, 0, 0, ...
                (1 - a)/2 + (-1 + a^2)/2, 1 - a^2];
        dna = [1/2 + a, 0, 0, 0, 0, 0, -1/2 + a, -2*a];
    end
    dxa = dna*coord(:,1); dya = dna*coord(:,2);
    Jc=sqrt(dxa^2 + dya^2);
    ka = ka - alpha*Jc*w*n'*n;
    rb = rb + beta*Jc*w*n';
end
