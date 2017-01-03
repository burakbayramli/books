function p = intp(xj,y,xi)
%INTP        (n-1)th order polynomial Lagrange interpolation.
%            n = length(y). xj is an n by 1 vector of distinct 
%            data points, and y is an n by 1 vector of data 
%            given at xj.
%
%            See
%            Horn & Johnson: Matrix Analysis,
%            Cambridge University Press 1985, Vol. 1, 29--30

% Written by Kai Borre
% December 30, 2000

%y = [13; 17; 85];  % numerical example from B. Hofmann-Wellenhof
                    % et al., p. 71--72
%xj = [-3;1;5];     
%xi = 4;            
%y = [1;3;2];       % numerical example from Stoer, p. 33
%xj = [0;1;3];
%xi = 2;

for i = 1:length(y)
    pronum = [xi-xj];
    pronum(i) = [];
    num = prod(pronum);
    prodenom = [];
    for j = 1:length(y)
        prodenom = [prodenom; xj(i)-xj(j)];
        if i == j, prodenom(i) = []; end
    end
    denom = prod(prodenom);
    L(i) = num/denom;
end
p = L*y;

%%%%%%%%%%%%%%%%%%% end intp.m  %%%%%%%%%%%%%%%%


