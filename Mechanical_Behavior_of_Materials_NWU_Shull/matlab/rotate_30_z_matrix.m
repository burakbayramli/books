sig=[4,3,0; 3,1,2; 0,2,6]*10^6;
phi=30; 
theta=[phi,90-phi ,90;90+phi,phi ,90;90,90,0];  % this is for rotation about z axis
Q=cosd(theta);
QT=transpose(Q);
sigp=Q*sig*QT
