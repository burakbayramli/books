function [points, N] = CreatePoints(N,s,gridtype)
% Computes a set of N points in [0,1]^s
% Note: could add variable interval later
% Inputs:
% N: number of interpolation points
% s: space dimension
% gridtype: 'c'=Chebyshev, 'f'=fence(rank-1 lattice),
%    'h'=Halton, 'l'=latin hypercube, 'r'=random uniform, 
%    's'=Sobol, 'u'=uniform grid
% Outputs:
% points: an Nxs matrix (each row contains one s-D point)
% N: might be slightly less than original N for 
%    Chebyshev and gridded uniform points
% Calls on: chebsamp, lattice, lhsamp, gridsamp
% Also needs: fdnodes, gaussj
% Requires Statistics Toolbox for haltonset and sobolset.

switch gridtype
    case 'c'
        ppd = zeros(1,s);
        for j=1:s
            ppd(j) = floor(nthroot(N,s+1-j));
            N = N/ppd(j);
        end
        gam = 0.5*ones(1,s);  % density for point distribution, 0.5=Chebyshev
        points = chebsamp([zeros(1,s); ones(1,s)], ppd, gam);
        N = prod(ppd);
    case 'f'
        points = lattice(N,s);  % N should be(?) power of 2
    case 'h' 
        temp = haltonset(s);
        points = net(temp,N);
    case 'l'
        points = lhsamp(N,s);
    case 'r'
        rand('state',47); 
        points = rand(N,s);
    case 's'
        temp = sobolset(s);
        points = net(temp,N);
        points = N*points/(N-1);        
    case 'u'
        ppd = zeros(1,s);
        for j=1:s
            ppd(j) = floor(nthroot(N,s+1-j));
            N = N/ppd(j);
        end
        points = gridsamp([zeros(1,s); ones(1,s)], ppd);
        N = prod(ppd);
    otherwise
        error('Please use c, f, h, r, s or u data types')
end
