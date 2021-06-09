%
fprintf(fid, ' ******* PRINTING MODEL DATA **************\n\n\n');

% Print Nodal coordinates
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Number of nodes:                                 %g\n', nnd );  
fprintf(fid, 'Number of elements:                              %g\n', nel ); 
fprintf(fid, 'Number of nodes per element:                     %g\n', nne ); 
fprintf(fid, 'Number of degrees of freedom per node:           %g\n', nodof); 
fprintf(fid, 'Number of degrees of freedom per element:        %g\n\n\n', eldof); 
%
%
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Node        X            Y \n');
for i=1:nnd
fprintf(fid,' %g,      %07.2f,      %07.2f\n',i, geom(i,1), geom(i,2));
end
fprintf(fid,'\n');
%
% Print element connectivity
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Element      Node_1      Node_2 \n');
for i=1:nel
fprintf(fid,'    %g,         %g,          %g\n',i, connec(i,1), connec(i,2));
end
fprintf(fid,'\n');
%
% Print element property
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Element        E             A \n');
for i=1:nel
fprintf(fid,'    %g,       %g,       %g\n',i, prop(i,1), prop(i,2));
end
fprintf(fid,'\n');
%
% Print Nodal freedom
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Node      disp_U     disp_V\n');
for i=1:nnd
fprintf(fid,'  %g,        %g,          %g\n',i, nf(i,1), nf(i,2));
end
fprintf(fid,'\n');
%
% Print Nodal loads
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Node      load_X         load_Y\n');
for i=1:nnd
fprintf(fid,'  %g,      %07.2f,        %07.2f\n',i, load(i,1), load(i,2));
end
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid,'\n');
fprintf(fid,'Total number of active degrees of freedom, n = %g\n',n);
fprintf(fid,'\n');
%