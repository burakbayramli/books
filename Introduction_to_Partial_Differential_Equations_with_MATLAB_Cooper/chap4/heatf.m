


function y = heatf(x)
%
%  This mfile contains all the initial data choices for the three programs
%  heat3, heat4, and heat5. Remove the % sign from in front of the
%  formula you want to use.  Then replace it before making the 
%  data choice.


%                 Initial Data Choices for heat3

%    1)
%     y = -sin(2*pi*x/10.0).*exp(.5*x);

%    2)
%    y1 = x;  y2 = 10 -x;  y = (x < 5.001).*y1 + (x > 5).*y2 ;

%    3)
%    y = .5*x - 2 + sin(.2*pi*x);

%    4)
%    y = zeros(size(x));
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%                 Initial Data Choices for heat4

%   1)
%   y = exp(-(x-5).^2) ;

%    2)
%    y = .01*(.25*x.^4 - (17*x.^3)/3 + 35*x.^2 ) - 1 ;

%   3) and 4)
%   y = zeros(size(x));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%                 Initial Data Choices for heat5

%%   1) 
%   y = sin(pi*x/10);

%   2)
%   y = exp(-2*(x-7.5).^2) ;

%   3)
   y = .5*x - 2 + 3*exp(-.5*(x-5).^2);

%   4)
%   y = zeros(size(x));

  

