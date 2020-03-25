function S=score(Z,Zb,x,c,A,a)
% implements the portfolio loss function
X=A*Z'+a.*Zb';
S=c*(X>x');