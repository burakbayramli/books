% Mohr's circle derivation - beginning from previous calculation of sigp
center=(sig1p+sig2p)/2;  % the center of Mohr's circle
radius=(sig1p-sig2p)/2;  % radius of Mohr's circle
% now we shift the origin to the center of Mohr's circle, and 
% normalize by the radius of Mohrs circle
sigp(1,1)=(sigp(1,1)-center)/radius;
sigp(2,2)=(sigp(2,2)-center)/radius;
% shear stresses are plotted on the y axis, so we don't need to shift the
% origin, we just need to normlized by the radius
sigp(1,2)=sigp(1,2)/radius;
sigp(2,1)=sigp(1,2);
simplify(sigp)

