% choose_sat.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module allowing the selection of specific satellites or use of the    %
% default set.                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
default=input('Use default satellite set? (Y or N)  :','s');			% get response
if (default=='y')|(default=='Y')
   sat_set = [1 2 3 4];
else
   disp(' ');
   disp('Choose four of the following five satellites:');
   disp(' ');
   disp('Satellite No.       Omega_0 (deg)      Theta_0 (deg)');
   disp('    1                  326                68');
   disp('    2                   26               340');
   disp('    3                  146               198');
   disp('    4                   86               271');
   disp('    5                  206                90');
   disp(' ');
   sat_set=input('Enter four satellite numbers as a row vector (Ex. [1 2 3 5]):');
end
