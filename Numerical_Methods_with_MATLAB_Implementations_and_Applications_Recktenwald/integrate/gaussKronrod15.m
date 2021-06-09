function [I,abserr] = gaussKronrod15(fun,a,b,varargin)
% gaussKronrod15  Gauss-Kronrod quadrature pair of order 7 and 15
%
% Synopsis:   I         = gaussKronrod15(f,a,b)
%            [I,abserr] = gaussKronrod15(f,a,b)
%
% Input:     f = (string) name of m-file that evaluates the integrand, f(x)
%            a,b = lower and upper limits of the integral
%            arg1,arg2,... = optional arguments passed through to fun
%
% Output:    I = approximation to integral of f(x)*dx from a to b
%                Obtained with 15 point Kronrod rule using optimal
%                addition to the result from 7 point Gauss rule
%            abserr = (optional) estimate of absolute error.  Obtained
%                     as difference between result obtained with 15 pt
%                     Kronrod rule and 7 point Gauss rule

% Notes:
%   Code from QUADPACK by Piessens et al., translated to MATLAB by
%   G. Recktenwald.  Some output options eliminated in translation.
%   Numerical values of nodes and weights copied from dqc15.f source.
%
% From QUADPACK notes:
%   Gauss quadrature weights and Kronrod quadrature abscissae and weights
%   as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
%   Bell Labs, nov. 1981.

wg = [ 0.129484966168869693270611432679082;   %  weights of 7 point Gauss rule
       0.279705391489276667901467771423780;
       0.381830050505118944950369775488975;
       0.417959183673469387755102040816327 ];

xgk = [ 0.991455371120812639206854697526329;  %  nodes of 15 point Kronrod rule
        0.949107912342758524526189684047851;  %  xgk(2:2:14) are nodes of 7 point
        0.864864423359769072789712788640926;  %  Gauss rule
        0.741531185599394439863864773280788;
        0.586087235467691130294144838258730;
        0.405845151377397166906606412076961;
        0.207784955007898467600689403773245;
        0                                   ];

wgk  =[ 0.022935322010529224963732008058970;  %  weights of 15 point Kronrod rule
        0.063092092629978553290700663189204;
        0.104790010322250183839876322541518;
        0.140653259715525918745189590510238;
        0.169004726639267902826583426598550;
        0.190350578064785409913256402421014;
        0.204432940075298892414161999234649;
        0.209482141084727828012999174891714 ] ;

half = 0.5*(b-a);                                  %  half length of the interval
x = 0.5*(a+b) + half*[ -xgk(1:8); xgk(7:-1:1) ];   %  Translate 15 nodes to interval (a,b)
f = feval(fun,x,varargin{:});                      %  Evaluate f at all 15 nodes

gsum = half*( sum( wg(1:3).*f(2:2:6) ) + sum( wg(4:-1:1).*f(8:2:14) ) );  %  7 pt Gauss 
I    = half*( sum(wgk(1:8).*f(1:8) )   + sum(wgk(7:-1:1).*f(9:15)   ) );  % 15 pt Kronrod

if nargout==2,  abserr = abs(I-gsum);  end
