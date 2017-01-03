function demoSiCmod
% demoSiCmod  Least squares fit of bulk modulus of SiC versus temperature
%
% Synopsis:  demoSiCmod
%
% Input:   none
%
% Output:  Plot of line fit to data
[t,D,labels] = loadColData('SiC.dat',6,5);
g  = D(:,1);    %  bulk modulus (GPa) in first column

[c,R2] = linefit(t,g);
fprintf('\nLine fit to Bulk Modulus of SiC versus temperature\n');
fprintf('\tc(1) = %14.6e\n\tc(2) = %14.6e\n\tR2   =   %6.4f\n',c,R2)

tfit = [ min(t) max(t)];    gfit = c(1)*tfit + c(2);   % evaluate fit function

plot(t,g,'o',tfit,gfit,'-');
xlabel('T   {}^\circ C');     ylabel('Bulk Modulus    GPa');
