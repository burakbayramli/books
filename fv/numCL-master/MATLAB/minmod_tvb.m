function m = minmod_tvb(a,b,c,dx,M)

% if |a|<=M*dx^2, then return the first argument, otherwise apply the usual
% minmod function
a1 = (abs(a) <= M*dx^2);
a2 = (abs(a) > M*dx^2);

m = a1.*a + a2.*(sign(a)==sign(b) & sign(b)==sign(c) & sign(c)==sign(a)).*sign(a).*min(abs([a b c]),[],2);

return