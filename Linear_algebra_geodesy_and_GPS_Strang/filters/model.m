function model(ofile,navfile)
%MODEL  Receiver clock offset OS from kalclock is modeled;
%   	  first by a linear, next by a quadratic approximation.
%	     The model is subtrated from OS. The autocorrelation
%	     function for the residuals is plotted.

%Kai Borre 03-22-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22 $

OS = kalclock(ofile,navfile,1);  % OS is a row vector
OS = OS - mean(OS);
s = size(OS,2);
% Begin with a linear model for the receiver clock offset
a = OS(1);
b = OS(s);
for i = 1:s
   of(i) = OS(i) - (a+(i-1)*(b-a)/(s-1));
end
% We model the clock offset by a second order polynomial:
t = (1:s)';
A = [ones(s,1) t t.^2];
x = inv(A'*A)*A'*OS';
for i = 1:s
   os(i) = OS(i)- (x(1)+i*x(2)+i^2*x(3));
end

fprintf('\n Clock offset polynomial: %6.3ft^2 + %7.3ft + %8.3f\n', ...
                                                    x(3), x(2), x(1));
subplot(2,2,1), plot(of), title('Linear trend subtracted')
subplot(2,2,2), autocorr(of)
subplot(2,2,3), plot(os), title('Quadratic trend subtracted')
subplot(2,2,4), autocorr(os)
toptitle('Receiver clock offset')
%%%%%%%%%%%% end model.m  %%%%%%%%%%%%%%%%%%%%%%
