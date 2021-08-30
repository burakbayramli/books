clear all % clear the workspace of previously defined variables
sig=1e6*[2.5,2.5,0;2.5,2.5,0;0,0,0];
[directions,principalstresses]=eigs(sig);
% the columns in 'directions' correspond to the dot product of the
% principal axes with the orignal coordinate system
% The rotation angles are obtained by calculating the inverse cosines
theta=acosd(directions)
principalstresses
