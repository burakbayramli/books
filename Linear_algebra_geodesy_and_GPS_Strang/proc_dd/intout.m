function intout(logid, text, ivec, l1, l2)
% INTOUT   spools integer vector to a file

% logid    file identification number
% text     text to accompany the data
% ivec     integer vector to be spooled
% l1       first element of vector to be spooled
% l2       last element of vector to be spooled

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

fwrite(logid, text);
fprintf(logid,'\n');
for i = l1:l2
   fprintf(logid,'%12.0f \n', vec(i));
end
%%%%%%%%%%%% end intout.m %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
