% This tests the support vector machine content that appears in the book
% After running this, h will contain the figure handle of the plot that is
% created.  If two plots are created, h1 and h2 are the figure handles.
% This is a test of the cross-validation as a measure of optimality

% To allow for the low-rank expansion parameter to be set
global GAUSSQR_PARAMETERS

% Choose a range of ep and bc values
epvec = logspace(-2,2,30);
bcvec = logspace(-2,4,31);

% Choose the number of cross-validations to compute
cv_fold = 10;

% Use the low rank matrix multiplication strategy
low_rank = 0;
GAUSSQR_PARAMETERS.DEFAULT_REGRESSION_FUNC = .05;

% Choose whether to compute 1D or 2D plots
plot2D = 1;

% Define the size of the problem
test_N = 10;
train_N = 100;

% Create random training and test data
% For this test (design_opt=1), we use grnmean=(1,0) and redmean=(0,1)
[train_data,train_class,test_data,test_class] = SVM_setup(1,train_N,test_N);

if plot2D
    % Produce a 2D plot for a range of ep and bc values
    cvmat = zeros(length(epvec),length(bcvec));
    errmat = zeros(length(epvec),length(bcvec));
    k_ep = 1;
    h_waitbar = waitbar(0,'Initializing');
    for ep=epvec
        k_bc = 1;
        for bc=bcvec
            cvmat(k_ep,k_bc) = gqr_svmcv(cv_fold,train_data,train_class,ep,bc,low_rank);
            SVM = gqr_fitsvm(train_data,train_class,ep,bc,low_rank);
            errmat(k_ep,k_bc) = sum(test_class ~= SVM.eval(test_data));
            
            progress = floor(100*((k_ep-1)*length(bcvec)+k_bc)/(length(epvec)*length(bcvec)))/100;
            waitbar(progress,h_waitbar,sprintf('%d-fold CV, \\epsilon=%5.2f C=%5.2f',cv_fold,ep,bc))
            k_bc = k_bc + 1;
        end
        k_ep = k_ep + 1;
    end
    
    waitbar(1,h_waitbar,'Plotting')
    [E,B] = meshgrid(epvec,bcvec);
    
    h1 = figure;
    h_ev = surf(E,B,cvmat');
    set(h_ev,'edgecolor','none')
    set(gca,'xscale','log')
    set(gca,'yscale','log')
    set(gca,'ytick',[1e-2,1e1,1e4])
    xlabel('\epsilon')
    ylabel('C')
    zlim([.9,1.1])
    zlabel(sprintf('%d-fold CV residual',cv_fold))
    shading interp
    grid off
    view([-.7,1,1])
    
    h2 = figure;
    h_err = surf(E,B,errmat');
    set(h_err,'edgecolor','none')
    set(gca,'xscale','log')
    set(gca,'yscale','log')
    set(gca,'ytick',[1e-2,1e1,1e4])
    set(gca,'ztick',[0,5,10])
    xlabel('\epsilon')
    ylabel('C')
    zlim([0,10])
    zlabel('missed classifications')
    shading interp
    grid off
    view([-.7,1,1])
    
    close(h_waitbar)
else
    % Produce a plot studying the associated CV residuals with one parameter
    % fixed and the other allowed to vary
    % We could allow for other ep and bc values, but whatever
    errvec_ep = zeros(size(epvec));
    errvec_bc = zeros(size(bcvec));
    k = 1;
    h_waitbar = waitbar(0,'Initializing');
    bc = 1;
    for ep=epvec
        errvec_ep(k) = gqr_svmcv(cv_fold,train_data,train_class,ep,bc,low_rank);
        k = k + 1;
        progress = floor(100*k/(length(bcvec)+length(epvec)))/100;
        waitbar(progress,h_waitbar,'Computing \epsilon CV')
    end
    k = 1;
    ep = 1;
    for bc=bcvec
        errvec_bc(k) = gqr_svmcv(cv_fold,train_data,train_class,ep,bc,low_rank);
        k = k + 1;
        progress = floor(100*(k+length(epvec))/(length(bcvec)+length(epvec)))/100;
        waitbar(progress,h_waitbar,'Computing C CV')
    end
    %         bc = 1;
    %         waitbar(progress,h_waitbar,'Optimality Search')
    %         [ep_opt,cv_opt] = fminbnd(@(ep)gqr_svmcv(cv_fold,train_data,train_class,ep,bc),.1,10);
    %         [cv_opt,opt_ind] = min(errvec_ep);
    %         ep_opt = epvec(opt_ind);
    waitbar(1,h_waitbar,'Plotting')
    h = figure;
    semilogx(epvec,errvec_ep,'linewidth',2)
    hold on
    semilogx(bcvec,errvec_bc,'--','linewidth',2)
    ylabel(sprintf('%d-fold CV residual',cv_fold))
    legend('C=1, \epsilon x-axis','\epsilon=1, C x-axis','location','northwest')
    hold off
    close(h_waitbar)
end