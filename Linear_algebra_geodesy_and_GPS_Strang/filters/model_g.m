function model_g(obs)
%MODEL_G   The data obs are modeled; obs is assumed to be a row vector!
%     	  first by a linear, next by a quadratic approximation.
%	        The model is subtrated from obs leaving residuals the
%	        autocorrelation function of which is plotted

%Kai Borre 03-22-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global tt of

e = exist('model.eps');
if e ~= 0
   delete model.eps
end;

obs = obs - mean(obs);
s = size(obs,2);
% Begin with a linear model for the data
a = obs(1);
b = obs(s);
for i = 1:s
   of(i) = obs(i) - (a+i*(b-a)/s);
end
% We model the data by a second order polynomium
t = (1:s)';
A = [ones(s,1) t t.^2];
x = inv(A'*A)*A'*obs';
for i = 1:s
   os(i) = obs(i)- (x(1)+i*x(2)+i^2*x(3));
end

tt = 0:s-1;
fprintf('\n Polynomium: %6.3ft^2 + %7.3ft + %8.3f\n', ...
                                           x(3), x(2), x(1));
subplot(2,2,1), plot(tt,of), set(gca,'Fontsize',14)
title('Linear trend subtracted','FontSize',14)
subplot(2,2,2), autocorr(of')
% set(gca,'Visible','off');
subplot(2,2,3), plot(tt,os), set(gca,'Fontsize',14)
title('Quadratic trend subtracted','FontSize',14)
subplot(2,2,4), autocorr(os')
% set(gca,'Box','on','Xtick',[],'Visible','on');

print model -deps
%%%%%%%%%%%% end model_g.m  %%%%%%%%%%%%%%%%%%%%%%
