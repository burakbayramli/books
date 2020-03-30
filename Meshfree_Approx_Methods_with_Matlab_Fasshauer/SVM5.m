% This runs time tests on the SVM solver with the full rank and low rank
% representations.  It also compares the cost of the sparse SVM evaluations
% to ignorantly performing a full summation, which would be the associated
% cost of using an RBF network.  There is also an option to study the cost
% of different parameterizations for solving the quadratic program.

% To allow for the low-rank expansion parameter to be set
global GAUSSQR_PARAMETERS

% Choose a range of parameters to test over, or fixed parameters if testing
% over something else
epvec = logspace(-4,2,41);
bcvec = logspace(-4,4,40);
ep = .01;
bc = 1;

% Choose whether or not to plot the output, which you may not want to do if
% running the file remotely or within a batch script
% Set to 'off' to prevent plotting, 'on' to plot
% You can see a non-visible figure with handle h using figure(h)
% If this is 'off', instead of displaying the plot it automatically saves
% the figure using gqr_savefig
plots_on = 'on';

% Set the low rank parameter if desired
low_rank = 0;
GAUSSQR_PARAMETERS.DEFAULT_REGRESSION_FUNC = .1;

% Choose which test you want to run
% 1 - Fixed parameterization, increasing size, low-rank vs. standard
% 2 - Cost of solving optimization problem with various ep and bc values
test_opt = 2;

switch test_opt
    case 1
        % Create random training and test data
        train_N_vec = round(logspace(1,3.5,30));
        
        lowvec = zeros(length(train_N_vec),1);
        lowvecS = zeros(length(train_N_vec),1);
        fullvec = zeros(length(train_N_vec),1);
        fullvecS = zeros(length(train_N_vec),1);
        k = 1;
        h_waitbar = waitbar(0,'Initializing','Visible',plots_on);
        fprintf('LOW\t prep setup solve clean \tFULL\t prep setup solve clean\n')
        for train_N=train_N_vec
            [train_data,train_class] = SVM_setup(1,train_N,10);
            
            SVMlow = gqr_fitsvm(train_data,train_class,ep,bc,1);
            lowvec(k) = SVMlow.solve_time;
            lowvecS(k) = SVMlow.prep_time + SVMlow.setup_time + SVMlow.postsolve_time;
            
            SVMfull = gqr_fitsvm(train_data,train_class,ep,bc,0);
            fullvec(k) = SVMfull.solve_time;
            fullvecS(k) = SVMfull.prep_time + SVMfull.setup_time + SVMfull.postsolve_time;
            
            progress = floor(100*k/length(train_N_vec))/100;
            waitbar(progress,h_waitbar,sprintf('num points=%d, low rank time=%5.2f, full rank time=%5.2f',train_N,lowvec(k),fullvec(k)))
            if strcmp(plots_on,'off')
                fprintf('%5.2f%% complete, num points=%d, low rank time=%5.2f, full rank time=%5.2f\n',progress*100,train_N,lowvec(k),fullvec(k))
            else
                fprintf('\t%5.2g %5.2g %5.2g %5.2g \t\t %5.2g %5.2g %5.2g %5.2g\n',...
                    SVMlow.prep_time,SVMlow.setup_time,SVMlow.solve_time,SVMlow.postsolve_time,...
                    SVMfull.prep_time,SVMfull.setup_time,SVMfull.solve_time,SVMfull.postsolve_time)
            end
            k = k + 1;
        end
        
        waitbar(1,h_waitbar,'Plotting')
        h = figure('Visible',plots_on);
        loglog(train_N_vec,lowvec,'linewidth',2)
        hold on
        loglog(train_N_vec,fullvec,'--','linewidth',2)
        xlabel('number of training points')
        ylabel('training time')
        legend('Low rank','Full rank','location','southeast')
        hold off
        if strcmp(plots_on,'off')
            gqr_savefig(h,sprintf('SVMtimetests%d',test_opt));
            plot_command = 'loglog(train_N_vec,[lowvec,fullvec])';
            save(sprintf('SVMtimetests%d',test_opt),'train_N_vec','lowvec','fullvec','plot_command');
        end
        
        close(h_waitbar)
    case 2
        % Create random training and test data
        [train_data,train_class] = SVM_setup(1,400,10);

        T = zeros(length(epvec),length(bcvec));
        k_ep = 1;
        h_waitbar = waitbar(0,'Initializing','Visible',plots_on);
        fprintf('  ep   bc  prep setup solve clean\n')
        for ep=epvec
            k_bc = 1;
            for bc=bcvec
                SVM = gqr_fitsvm(train_data,train_class,ep,bc,low_rank);
                T(k_ep,k_bc) = SVM.solve_time;
                
                progress = floor(100*((k_ep-1)*length(bcvec)+k_bc)/(length(epvec)*length(bcvec)))/100;
                waitbar(progress,h_waitbar,sprintf('compute time=%5.2f, \\epsilon=%5.2f C=%5.2f',T(k_ep,k_bc),ep,bc))
                if strcmp(plots_on,'off')
                    fprintf('%5.2f%% complete, ep=%5.2f C=%5.2f, time=%5.2f\n',progress*100,ep,bc,T(k_ep,k_bc))
                else
                    fprintf('%5.2g %5.2g %5.2g %5.2g %5.2g %5.2g\n',...
                        ep,bc,SVM.prep_time,SVM.setup_time,SVM.solve_time,SVM.postsolve_time)
                end
                k_bc = k_bc + 1;
            end
            k_ep = k_ep + 1;
        end
        
        waitbar(1,h_waitbar,'Plotting')
        % Average values, to try to avoid fluctuations
        T_avg = 1/9*(T(1:end-2,1:end-2) + T(2:end-1,1:end-2) + T(3:end,1:end-2)+...
                     T(1:end-2,2:end-1) + T(2:end-1,2:end-1) + T(3:end,2:end-1)+...
                     T(1:end-2,3:end)   + T(2:end-1,3:end)   + T(3:end,3:end));
        [E,B] = meshgrid(epvec,bcvec);
        E_avg = E(2:end-1,2:end-1);
        B_avg = B(2:end-1,2:end-1);
        
        h = figure('Visible',plots_on);
        h_ev = surf(E_avg,B_avg,T_avg');
        set(h_ev,'edgecolor','none')
        set(gca,'xscale','log')
        set(gca,'yscale','log')
        set(gca,'xtick',[1e-2,1e1,1e4])
        set(gca,'ytick',[1e-4,1e0,1e4])
        xlabel('\epsilon')
        ylabel('C')
        zlabel('SVM training time')
        shading interp
        grid off
        view([.5,-1,.8])
        %colormap gray
        colorbar
        if strcmp(plots_on,'off')
            gqr_savefig(h,sprintf('SVMtimetests%d',test_opt));
            plot_command = 'surf(E_avg,B_avg,T_avg'')';
            save(sprintf('SVMtimetests%d',test_opt),'E_avg','B_avg','T_avg','plot_command');
        end
        
        close(h_waitbar)
    otherwise
        error('Unacceptable test case test_opt=%d',test_opt)
end