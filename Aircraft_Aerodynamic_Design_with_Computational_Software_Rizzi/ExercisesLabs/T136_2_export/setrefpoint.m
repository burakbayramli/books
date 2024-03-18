function[ref_point]=setrefpoint(ref_point,ref)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage: [REF_POINT] = setrefpoint (REF_POINT,REF)
%
% Allows the user to manipulate the reference point position through
% a simple interface. The [x,y,z] position of the REF_POINT is either set
% directly, or as a percentage of the MAC in ther structure REF (Ref.C_mac)
%
% Example:
%
%  [ref_point]=setrefpoint(ref_point,ref);
%
% Calls:
%       questions       Contain user interface queries in string
%                       format. 
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
no=questions(5);
if isempty(no)
   no=4;
   error(9)
end
disp(' ');
switch no
	case 1
   	%move in x
   	ref_point(1)=input('Move reference point to x= 	');
    
	case 2
   	%move in y
	ref_point(2)=input('Move reference point to y=	');
   	
	case 3
  		%move in z
  	 	ref_point(3)=input('Move reference point z= 	');
    case 4
      %Move ref point to percentage of MAC
      point=input('Move reference point to percent of MAC  [ % ] : 	');
      ref_point=ref.mac_pos+[point*ref.C_mac/100 0 0];
     
	otherwise
end
end%function