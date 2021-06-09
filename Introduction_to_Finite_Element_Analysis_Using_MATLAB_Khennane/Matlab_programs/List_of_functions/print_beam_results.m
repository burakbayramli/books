%
fprintf(fid, '-------------------------------------------------------- \n');
fprintf(fid, ' \n\n\n ******* PRINTING ANALYSIS RESULTS **************\n\n\n');
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
fprintf(fid, 'Node      disp_y        rotation\n');
for i=1:nnd
fprintf(fid,' %g,     %8.5f,      %8.5f\n',i, node_disp(i,1), node_disp(i,2));
end
fprintf(fid,'\n');
%
% Print Members actions
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Members actions \n');
fprintf(fid, 'element      fy1         M1           Fy2            M2\n');
for i=1:nel
    fprintf(fid,' %g,     %9.2f,    %9.2f,    %9.2f,    %9.2f\n',i, ...
                force(i,1),force(i,2),force(i,3),force(i,4));
end