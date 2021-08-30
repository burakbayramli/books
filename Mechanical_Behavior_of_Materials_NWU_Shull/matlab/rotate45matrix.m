%rotate45_matrix.m
sig=zeros(3); % create stress tensor and set to zero
sig(1,1)=5e6; % this is the only nonzero component
phi=45; 
theta=[phi,90-phi,90;90+phi,phi,90;90,90,0];
Q=cosd(theta);
QT=transpose(Q);
sigp=Q*sig*QT

