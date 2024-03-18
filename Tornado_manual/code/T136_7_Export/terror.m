%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL 
% PUBLIC LICENSE.TXT.  If not, write to the Free Software 
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: [-] =  TERROR(A)
%
% Displays the error messages in startup of Tornado.
% A is the error number to display.
%
% Example:
%
%  answ=questions(1);       %question string generator function
%   if isempty(answ)
%      answ=10;
%      terror(9)             %Error message string generator function
%   end
%
% Calls:
%       none
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Error messages.
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header.   TM
%              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function []=terror(no)
disp('+++')
disp(' ')
switch no
case 0
case 1
   disp('ERROR: No state selected, void solution. ')
   disp('No calculation made, redo from start.')
case 2
   disp('ERROR: rudder index out of bounds.')   
   disp('No new setting set, redo from start.')
case 3
   disp('ERROR: No data.')   
   disp('Nothing to plot, redo from start.')
case 4
   disp(' ')
   disp('ERROR: No such file')
   disp(' ')
   errordlg( 'ERROR: No such file' )
case(5)
   disp(' ')
   disp('ERROR: Nothing to save')
   disp(' ')   
case 8
   disp(' ')
   disp('ERROR: unspecified user-error catch')
   disp(' ')
case 9
   disp('SYNTAX ERROR: Choice not in list. ') 
   disp('No new selection made, redo from start.')
case 10
   disp(' ')
   disp('ERROR: No geometry loaded')
   disp(' ')
case 11
   disp(' ')
   disp('ERROR: Lattice inconsistent with geometry')
   disp('Please regenerate lattice. ')
   disp(' ')   
case 12
   disp(' ')
   disp('ERROR: No such JID, no file to load.')
   disp(' ')
case 13
   disp(' ')
   disp('Warning: Airspeed=0, division by zero. Please set airspeed!=0')
   disp('No Wake Created ')
case 14
   disp(' ')
   disp('ERROR: Atmosphere lookup error, state set to SSL, please redo.')
   disp(' ')
   disp('Altitude must be between 0 (zero) and 70000 feet.')
   disp(' ')
case 15
   disp(' ')
   disp('CAUTION: Compressible Effects.')
   disp(' ') 
case 16
   disp(' ')
   disp('Warning, Geometry not symmetric. Solver Aborted.')
   disp(' ')
case 17
   disp(' ')
   disp('Warning, flow condidion symmetric. Solver Aborted.')
   disp(' ')
   
case 18
 
   disp(' ')
   disp('No trim axis selected. Solver Aborted.')
   disp(' ')
   
case 19
 
   disp(' ')
   disp(' Only one trim effector may be used per simulation, Solver Aborted.')
   disp(' ')
   
case 20
   disp(' ')
   disp(' Input error, Solver Aborted.')
   disp(' ')
   
   case 21 %***
    disp(' ') %***
    disp('Warning, invalid case selection. ') %***
    disp('Defaulting to fixed coefficients.') %***
    disp(' ') %***
   
    case(22)
       disp(' ')
       disp(' Zero thickness aifoil, drag computation aborted.')
       disp(' ')
       
    case(23)
       disp(' ')
       disp(' No wing data: No lattice created.')
       disp(' ')
       
    case(24)
       disp(' No body data to compute on.');
       
    
otherwise
   disp('Unknown internal error, please report this bug')
end
disp(' ')