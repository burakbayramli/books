% BVP_field_Jpattern.m
% This MATLAB program demonstrates the sparsity patterns
% obtained for a 1-D transport-reaction problem
% based on the use of different stacking schemes.
% K.J. Beers. MIT ChE. 11/15/2004

function iflag = BVP_field_Jpattern(N_f,N_g);
iflag = 0;


N_tot = N_f*N_g;  % total # of unknowns
Nz_per_row = N_f + 2;  % # of non-zero diagonals
Nz = N_tot*Nz_per_row;  % # of non-zero elements

% scheme I, stacking c1(z1),...,c1(zN),c2(z1),...
S_1 = spalloc(N_tot,N_tot,Nz);
for i_g = 1:N_g
    for j_f = 1:N_f
        m = get_label_1(j_f,i_g,N_f,N_g);
        % each species at same point
        for k_f = 1:N_f
            n = get_label_1(k_f,i_g,N_f,N_g);
            S_1(m,n) = 1;
        end
        % same field, left point
        if(i_g > 1)
            n = get_label_1(j_f,i_g-1,N_f,N_g);
            S_1(m,n) = 1;
        end
        % same field, right point
        if(i_g < N_g)
            n = get_label_1(j_f,i_g+1,N_f,N_g);
            S_1(m,n) = 1;
        end
    end
end
% make spy plot of sparsity pattern of Jacobian
figure; subplot(1,2,1);
spy(S_1);
title('Scheme I');


% scheme II, stacking c1(z1),c2(z1),...,c1(z2),c2(z2),...
S_2 = spalloc(N_tot,N_tot,Nz);
for i_g=1:N_g
    for j_f = 1:N_f
        m = get_label_2(j_f,i_g,N_f,N_g);
        % each species at same point
        for k_f = 1:N_f
            n = get_label_2(k_f,i_g,N_f,N_g);
            S_2(m,n) = 1;
        end
        % same field, left point
        if(i_g > 1)
            n = get_label_2(j_f,i_g-1,N_f,N_g);
            S_2(m,n) = 1;
        end
        % same field, right point
        if(i_g < N_g)
            n = get_label_2(j_f,i_g+1,N_f,N_g);
            S_2(m,n) = 1;
        end
    end
end
% make spy plot of sparsity pattern of Jacobian
subplot(1,2,2);
spy(S_2);
title('Scheme II');





iflag = 1;
return;


% ========================
function label = get_label_1(j_f,i_g,N_f,N_g);

label = (j_f-1)*N_g + i_g;

return;


% ========================
function label = get_label_2(j_f,i_g,N_f,N_g);

label = (i_g-1)*N_g + j_f;

return;

