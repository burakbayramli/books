function template = ave_template(winsize)
%Template for direct averaging
%
%  Usage: [template] = ave_template(number)
%
%  Parameters: winsize    - size of template (odd, integer)
%
%  Author: Mark S. Nixon

%each coefficient is reciprocal of number of points
template(1:winsize,1:winsize)=1/(winsize*winsize);
