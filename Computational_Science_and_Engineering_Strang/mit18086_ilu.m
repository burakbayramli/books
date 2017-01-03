function mit18086_ilu(n)
%MIT18086_ILU
%    Sets up a 2d Poisson problem, and computes the LU
%    as well as ILU decomposition for different tolerances
%    (including ILU 0 and tol=0, which is normal LU).
%    Run times are clocked and errors are measured.

% 03/2008 by Benjamin Seibold
% Feel free to modify for teaching and learning.
%-------------------------------------------------------------------
tols = {'0' 1e-1 1e-3 1e-5 0}; % set of ILU tolerances
n_runs = 5;                    % number of runs for cpu-time
%-------------------------------------------------------------------
if nargin<1, n = 16; end
A = delsq(numgrid('S',n+2));   % 2d Poisson matrix for square
t = 1:n_runs; for k = 1:n_runs, tic, [L,U] = lu(A); t(k) = toc; end
t_lu = median(t);
disp('========================================')
disp(sprintf('cond(A)                 = %0.2f',condest(A)))
disp(sprintf('runtime LU              = %0.3f ms',t_lu*1e3))
disp(sprintf('norm(A-L*U)             = %0.2e',normest(A-L*U)))
disp('========================================')
clf
subplot(length(tols)+1,3,1), spy(A), title('2d Poisson matrix A')
subplot(length(tols)+1,3,2), spy(L,'b')
hold on, spy(U,'r'), hold off, title('LU factorization')
drawnow
for itol = 1:length(tols)
    for k = 1:n_runs
        tic, [Lapp,Uapp] = luinc(A,tols{itol}); t(k) = toc;
    end
    t_ilu = median(t);
    Aapp = Lapp*Uapp;
    for k = 1:n_runs, tic, Iapp = Aapp\A; t(k) = toc; end
    t_iapp = median(t);
    if tols{itol}=='0', name = 'ILU 0'; else
        name = sprintf('%0.1e',tols{itol});
    end
    disp(['tolerance               = ',name])
    disp('----------------------------------------')
    disp(sprintf('cond(inv(Aapp)*A)       = %0.2f',condest(Iapp)))
    disp(sprintf('runtime ILU             = %0.3f ms',t_ilu*1e3))
    disp(sprintf('runtime inv(Aapp)*A     = %0.3f ms',t_iapp*1e3))
    disp(sprintf('norm(A-Aapp)            = %0.2e',normest(A-Aapp)))
    disp(sprintf('norm(A-Aapp.*spones(A)) = %0.2e',...
         normest(A-Aapp.*spones(A))))
    disp('========================================')
    subplot(length(tols)+1,3,itol*3+1), spy(Lapp,'b'), hold off
    text(n^2*.9,n^2*.2,name,'HorizontalAlignment','right')
    title('L_{app}')
    subplot(length(tols)+1,3,itol*3+2), spy(Uapp,'r'), hold off
    title('U_{app}')
    subplot(length(tols)+1,3,itol*3+3), spy(Aapp,'b'), hold off
    title('L_{app}*U_{app}')
    drawnow
end
