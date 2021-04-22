%
fprintf(fid, '-------------------------------------------------------- \n');
fprintf(fid, ' \n ******* PRINTING ANALYSIS RESULTS ************\n\n');
%
% Print nodal displacements
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, 'Nodal displacements \n');
fprintf(fid, 'Node      disp_x          disp_y \n');

for i=1:nnd
fprintf(fid,' %g,     %8.5e,     %8.5e\n',  ...
               i, node_disp(i,1), node_disp(i,2));
end
fprintf(fid,'\n');
%
% Print element stresses
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, '                    Element stresses \n');
fprintf(fid, 'element    sigma_(xx)         sigma_(yy)         tau_(xy)\n');
%
for i=1:nel
    fprintf(fid,' %g,      %7.4e,       %7.4e,       %7.4e\n',i, ...
                SIGMA(i,1),SIGMA(i,2),SIGMA(i,3));
end
%
%
% Print element strains
%
fprintf(fid, '------------------------------------------------------ \n');
fprintf(fid, '                     Element strains \n');
fprintf(fid, 'element    epsilon_(xx)       epsilon_(yy)         gamma_(xy)\n');
%
for i=1:nel
    fprintf(fid,' %g,      %7.4e,       %7.4e,       %7.4e\n',i, ...
                EPS(i,1),EPS(i,2),EPS(i,3));
end