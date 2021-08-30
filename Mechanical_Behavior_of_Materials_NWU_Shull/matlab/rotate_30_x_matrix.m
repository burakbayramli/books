sig=[4,3,0; 3,1,2; 0,2,6]*10^6;
phi=30;
theta=[0, 90, 90; 90, phi, 90-phi; 90, 90+phi, phi]; % for rotation about x axis
Q=cosd(theta);
QT=transpose(Q);
sigp=Q*sig*QT
