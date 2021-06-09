%
fprintf(fid, '-------------------------------------------------------- \n');
fprintf(fid, ' \n\n\n ******* PRINTING ANALYSIS RESULTS ************\n\n\n');
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
fprintf(fid, 'Node      disp_x      disp_y        rotation\n');
for i=1:nnd
fprintf(fid,' %g,     %8.5e,     %8.5e,      %8.5e\n',i, ...
             node_disp(i,1), node_disp(i,2),node_disp(i,3));
end
fprintf(fid,'\n');
%
% Print Members actions
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Members actions in local coordinates \n');
fprintf(fid, 'element    fx1     fy1      M1      fx2      Fy2       M2\n');
for i=1:nel
    fprintf(fid,' %g,   %7.4f,   %7.4f,    %7.4f,   %7.4f,   %7.4f,   %9.4f\n',i, ...
                force_l(i,1),force_l(i,2),force_l(i,3),force_l(i,4),force_l(i,5),force_l(i,6));
end
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Members actions in global coordinates \n');
fprintf(fid, 'element    fx1     fy1      M1      fx2      Fy2       M2\n');
for i=1:nel
    fprintf(fid,' %g,   %7.4f,   %7.4f,    %7.4f,   %7.4f,   %7.4f,   %9.4f\n',i, ...
                force_g(i,1),force_g(i,2),force_g(i,3),force_g(i,4),force_g(i,5),force_g(i,6));
end