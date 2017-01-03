function dout(fidlog, text, vec, l1, l2)
% DOUT      spools vector to a file

% fidlog   file identification number
% text     text to accompany the data
% vec      vector to be spooled
% l1       first component of vector to be spooled
% l2       last component of vector to be spooled

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

fprintf(fidlog,'\n');
fprintf(fidlog, '%20s\n', text);
fprintf(fidlog,'\n');
for i = l1:l2
   fprintf(fidlog,'%12.3f \n', vec(i));
end
%%%%%%%%%%%% dout.m  %%%%%%%%%%%%%%%%%%%%%%
