function atimesv(a,v)
%       atimesv  displays a two-dimensional
%       plot of the scalar-vector product a*v,
%       where a is a real scalar and v is a two
%       dimensional real vector.
%
%       This function displays a plot of a*v
%       following the illustration of Figure 1.1 in G. Strang,
%       "Introduction to Linear Algebra."

% Written by T. A. Bryan on 2 June 1993

x=a*v;

hold off
clf
plot([0 v(1)],[0,v(2)],':',...
    [0 x(1)],[0,x(2)],'-')
hold on
text(v(1),v(2),'v')
text(x(1),x(2),[num2str(a) '*v'])

hold off


