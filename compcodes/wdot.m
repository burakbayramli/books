

% function mfile wdot.m needed for mfile flow2.m. This mfile, in turn,
% requires mfiles u.m and v.m for the components of the velocity.

function z = wdot(t,w)

   z = [u(w(1), w(2)); v(w(1), w(2)) ] ;
