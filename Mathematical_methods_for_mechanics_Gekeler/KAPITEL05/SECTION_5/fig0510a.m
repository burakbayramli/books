function fig0510a
% Figure Van der Pol, 3-dimensional

clf
disp(' Call first DEMO2-1 ')
load datenC2 XX YY OMGA MU
%MU
XX = [XX, XX(:,1)];
YY = [YY, YY(:,1)];
ZZ = OMGA*ones(1,size(YY,2));
surf (XX,ZZ,YY)
xlabel('x')
ylabel('y')
zlabel('z')

