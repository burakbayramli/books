function addvec(v,w)
%ADDVEC Illustrate the sum v + w of 2-dimensional vectors.
%
%       addvec(v,w) displays a plot of v + w
%       following Figure 1.2 in G. Strang, 
%       "Introduction to Linear Algebra."

%       Written by T. A. Bryan on 2 June 1993

v=v(:);     w=w(:);      % Ensure that both are vectors of length two
if (length(v)~=2) | (length(w)~=2)
   error('Both v and w must be vectors of length two')
end

x = v + w;
hold off
clf
plot([0 v(1)],[0, v(2)],'-',...
     [0 w(1)],[0, w(2)],'-',...
     [0 x(1)],[0, x(2)],'-')
hold on
text(v(1),v(2),'v')
text(w(1),w(2),'w')
text(x(1),x(2),'v+w')

plot([w(1) x(1)],[w(2) x(2)],':')
plot([v(1) x(1)],[v(2) x(2)],':')
hold off

