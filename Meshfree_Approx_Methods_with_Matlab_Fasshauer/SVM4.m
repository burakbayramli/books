% This tests the support vector machine content that appears in the book.
% Only the basic plots of the input data and a few contours are provided
% here.  h1, h2, and h3 are the associated figure handles.
% This test is a nonseparable pattern

% To allow for the low-rank expansion parameter to be set
global GAUSSQR_PARAMETERS

% Use the low rank matrix multiplication strategy
low_rank = 0;
GAUSSQR_PARAMETERS.DEFAULT_REGRESSION_FUNC = .05;

% Choose a range of ep and bc values
epvec = logspace(-2,2,30);
bcvec = logspace(-2,4,31);

% Choose the number of cross-validations to compute
cv_fold = 10;

% Define the size of the problem
test_N = 20;
train_N = 200;

% Create random training and test data
% For this test (design_opt=1), we use grnmean=(1,0), (0,1), (2,1) and
%                                      redmean=(0,0), (1,1), (2,0)
[train_data,train_class,test_data,test_class,h1] = SVM_setup(2,train_N,test_N);

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

h2 = figure;
h_ev = surf(E,B,cvmat');
set(h_ev,'edgecolor','none')
set(gca,'xscale','log')
set(gca,'yscale','log')
set(gca,'ytick',[1e-2,1e1,1e4])
xlabel('\epsilon')
ylabel('C')
zlim([.6,.75])
zlabel(sprintf('%d-fold CV residual',cv_fold))
shading interp
grid off
view([-.7,1,1])

h3 = figure;
h_err = surf(E,B,errmat');
set(h_err,'edgecolor','none')
set(gca,'xscale','log')
set(gca,'yscale','log')
set(gca,'ytick',[1e-2,1e1,1e4])
set(gca,'ztick',[0,10,20])
xlabel('\epsilon')
ylabel('C')
zlim([0,20])
zlabel(sprintf('missed classifications',cv_fold))
shading interp
grid off
view([-.7,1,1])

close(h_waitbar)