%
fprintf(fid, '-------------------------------------------------------- \n');
fprintf(fid, ' \n\n\n ******* PRINTING ANALYSIS RESULTS **************\n\n\n');
%
%
%
% Print global force vector
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid,'Global force vector  F \n');
fprintf(fid,'   %g\n',F);
fprintf(fid,'\n');
%
%
% Print Displacement solution vector
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid,'Displacement solution vector:  delta \n');
fprintf(fid,' %8.5f\n',delta);
fprintf(fid,'\n');
%
% Print nodal displacements
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Nodal displacements \n');
fprintf(fid, 'Node      disp_X        disp_Y\n');
for i=1:nnd
fprintf(fid,' %g,     %8.5f,      %8.5f\n',i, node_disp(i,1), node_disp(i,2));
end
fprintf(fid,'\n');
%
% Print Members actions
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Members actions \n');
fprintf(fid, 'element      force         action\n');
for i=1:nel
    if force(i) > 0 
        fprintf(fid,' %g,       %9.2f,       %s\n',i, force(i), 'Tension');
    else
        fprintf(fid,' %g,       %9.2f,       %s\n',i, force(i), 'Compression'); 
    end
end