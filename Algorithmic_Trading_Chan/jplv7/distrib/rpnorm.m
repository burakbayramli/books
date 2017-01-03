function x = rpnorm(N,m,s);

% RPNORM    Random numbers from the positive normal distribution.
%   RPNORM(N,M,S) is an N-by-1 vector with random entries, generated
%   from a positive normal distribution with mean M and standard
%   deviation S.
%
% (c) Vincent Mazet, 06/2005
% Centre de Recherche en Automatique de Nancy, France
% vincent.mazet@cran.uhp-nancy.fr
%
% Reference:
% V. Mazet, D. Brie, J. Idier, "Simulation of Positive Normal Variables
% using several Proposal Distributions", IEEE Workshop Statistical
% Signal Processing 2005, july 17-20 2005, Bordeaux, France. 



if s<0,  error('Standard deviation must be positive.'); end;
if N<=0, error('N is wrong.'); end;

x = [];     % Output vector
NN = N;

% Intersections
A  = 1.136717791056118;
mA = (1-A^2)/A*s;
mC = s * sqrt(pi/2);

while length(x)<NN,
	
	if m < mA,      % 4. Exponential distribution
		a = (-m + sqrt(m^2+4*s^2)) / 2 / s^2;
        z = -log(1-rand(N,1))/a;
        rho = exp( -(z-m).^2/2/s^2 - a*(m-z+a*s^2/2) );
	
	elseif m <= 0,  % 3. Normal distribution truncated at the mean
                    % equality because 3 is faster to compute than the 2
        z = abs(randn(N,1))*s + m;
        rho = (z>=0);
	
	elseif m < mC,  % 2. Normal distribution coupled with the uniform one
        r = (rand(N,1) < m/(m+sqrt(pi/2)*s));
        u = rand(N,1)*m;
        g = abs(randn(N,1)*s) + m;
        z = r.*u + (1-r).*g;
        rho = r.*exp(-(z-m).^2/2/s^2) + (1-r).*ones(N,1);
	
	else,           % 1. Normal distribution
        z = randn(N,1)*s + m;
        rho = (z>=0);
        
	end;
    
	% Accept/reject the propositions
	reject = (rand(N,1) > rho);
	z(reject) = [];
	if length(z)>0, x = [x ; z]; end;
    N = N-length(z);

end;