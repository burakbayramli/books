function auto = autocorr(a);
%AUTOCORR Calculation of autocorrelation function
%   	    for a given sequence of observations a

%Kai Borre 8-5-96
%Copyright (c) 1997 by Kai Borre
%$Revision 1.0 $  $Date: 1997/09/22 $

e = exist('autocorr.eps');
if e ~= 0
   delete autocorr.eps
end

if nargin == 0
   a = randn(300,1); 
end;
m = mean(a);
[n,o] = size(a);

for shift = 0:n-2
   sum = 0;
   for i = 1:n-shift
      sum = sum+(a(i)-m)*(a(i+shift)-m);
   end;
   auto(shift+1) = sum/(n-shift-1);
end;

% The rightmost values of the autocorrelation function
% are not reliable; we decide to omit the last 20%
to_plot = round(.8*size(auto,2));
q = auto(1:to_plot);

figure
hold on
bar(0:to_plot-1,q,'r-')
plot([0 to_plot],[0 0],'g-')
%title('Autocorrelation','Fontsize',14)
ylabel('[ ]-squared','FontSize',14)
%xlabel('Epochs, interval 20 seconds','FontSize',14)
set(gca,'FontSize',14)
hold off
print autocorr -deps

%%%%%%%%%%%%%% end autocorr.m %%%%%%%%%%%%%%%%%%%%%%%%%%%
