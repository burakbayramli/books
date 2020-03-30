% This tests the support vector machine content that appears in the book
% After running this, h will contain the figure handle of the plot that is
% created.  If two plots are created, h1 and h2 are the figure handles.
% Several options, described in plot_results, are available here

% To allow for the low-rank expansion parameter to be set
global GAUSSQR_PARAMETERS

% Choose a shape parameter or range of ep to test
% Also, choose a box constraint or range of bc to test
% The ep and box_constraint values are only needed for plot_results=1
ep = .01;
epvec = logspace(-2,2,31);
box_constraint = 10000;
%box_constraint = .6;
bcvec = logspace(-2,8,30);

% Use the low rank matrix multiplication strategy
low_rank = 1;
GAUSSQR_PARAMETERS.DEFAULT_REGRESSION_FUNC = .1;

% Whether or not the user wants the results plotted
%   0 - study of alpha values in low rank fit
%   1 - graphic display of predictions
%   2 - tests range of epsilon values
%   3 - tests range of box constraint values
%   4 - 3D plot of both epsilon and box constraint range
plot_results = 1;

% Define the size of the problem
test_N = 10;
train_N = 100;

% Create random training and test data
% For this test (design_opt=1), we use grnmean=(1,0) and redmean=(0,1)
[train_data,train_class,test_data,test_class] = SVM_setup(1,train_N,test_N);

% Plot the results, if requested
switch(plot_results)
    case 0
        K = exp(-ep^2*DistanceMatrix(train_data,train_data).^2);
        alphavec = logspace(0,8,30);
        errvec = zeros(size(alphavec));
        k = 1;
        for alpha=alphavec
            GQR = gqr_solveprep(1,train_data,ep,alpha);
            Phi1 = gqr_phi(GQR,train_data);
            Kpp = Phi1*diag(GQR.eig(GQR.Marr))*Phi1';
            errvec(k) = errcompute(Kpp,K);
            k = k + 1;
        end
        loglog(alphavec,errvec)
        xlabel('GQR alpha')
        ylabel('errcompute(low rank,K)')
        title(sprintf('ep=%4.2g,M=%d',ep,length(GQR.Marr)))
    case 1
        % Fit the SVM using the necessary parameters
        SVM = gqr_fitsvm(train_data,train_class,ep,box_constraint,low_rank);

        % Evaluate the classifications of the test data
        % Separate the correct classifications from the incorrect classifications
        predicted_class = SVM.eval(test_data);
        correct = predicted_class==test_class;
        incorrect = predicted_class~=test_class;
        
        % Find the decision contour
        d = 0.02;
        [CD1,CD2] = meshgrid(min(train_data(:,1)):d:max(train_data(:,1)),...
            min(train_data(:,2)):d:max(train_data(:,2)));
        contour_data = [CD1(:),CD2(:)];
        contour_class = SVM.eval(contour_data);

        % Plot the output of the SVM training
        % I could probably do this better with scatter, but oh well
        grnind = [ones(test_N,1);zeros(test_N,1)];
        redind = [zeros(test_N,1);ones(test_N,1)];
        h = figure;
        hold on
        plot(1,0,'gh','markersize',12,'MarkerFaceColor','g');
        plot(0,1,'rp','markersize',12,'MarkerFaceColor','r');
        plot(test_data(1:test_N,1),test_data(1:test_N,2),'g+','markersize',12)
        plot(test_data(test_N+1:2*test_N,1),test_data(test_N+1:2*test_N,2),'rx','markersize',12)
        plot(test_data(correct&grnind,1),test_data(correct&grnind,2),'sb','markersize',12)
        plot(test_data(incorrect&grnind,1),test_data(incorrect&grnind,2),'sc','markersize',12,'linewidth',2)
        plot(test_data(correct&redind,1),test_data(correct&redind,2),'ob','markersize',12)
        plot(test_data(incorrect&redind,1),test_data(incorrect&redind,2),'oc','markersize',12,'linewidth',2)
        plot(train_data(1:train_N,1),train_data(1:train_N,2),'g.')
        plot(train_data(train_N+1:2*train_N,1),train_data(train_N+1:2*train_N,2),'r.')
        plot(train_data(SVM.sv_index,1),train_data(SVM.sv_index,2),'ok','markersize',3)
        contour(CD1,CD2,reshape(contour_class,size(CD1)),[0 0],'k');
        hold off
    case 2
        % Test a bunch of ep values with a fixed box_constraint to see what the
        % results look like
        errvec = zeros(size(epvec));
        marvec = zeros(size(epvec));
        svmvec = zeros(size(epvec));
        k = 1;
        for ep=epvec
            SVM = gqr_fitsvm(train_data,train_class,ep,box_constraint,low_rank);
            errvec(k) = sum(test_class ~= SVM.eval(test_data));
            marvec(k) = SVM.margin;
            svmvec(k) = sum(SVM.sv_index);
            fprintf('%d\t%d\t%5.2f\t%d\n',k,svmvec(k),ep,SVM.bias)
            k = k + 1;
        end
        
        h1 = figure;
        semilogx(epvec,errvec,'linewidth',2)
        xlabel('\epsilon')
        ylabel('missed classifications')
        ylim([0,10])
        set(gca,'ytick',[0,5,10])
        
        h2 = figure;
        [AX,H1,H2] = plotyy(epvec,svmvec,epvec,marvec,'semilogx','loglog');
        xlabel('\epsilon')
        ylabel(AX(1),'support vectors')
        ylabel(AX(2),'margin')
        set(AX(1),'ycolor','k')
        set(AX(2),'ycolor','k')
        set(AX(1),'ylim',[0,200])
        set(AX(2),'ylim',[.01,1])
        set(AX(2),'xlim',[min(epvec),max(epvec)])
        set(AX(2),'xticklabel',{})
        set(AX(1),'ytick',[0,100,200])
        set(AX(2),'ytick',[.01,.1,1])
        set(H1,'color','k')
        set(H2,'color','k')
        set(H1,'linestyle','--')
        set(H1,'linewidth',2)
        set(H2,'linewidth',2)
        title(sprintf('C=%g',box_constraint))
        legend('# SV','Margin','location','east')
    case 3
        % Test a bunch of box_constraint values with a fixed ep to see what the
        % results look like
        errvec = zeros(size(bcvec));
        marvec = zeros(size(bcvec));
        svmvec = zeros(size(bcvec));
        k = 1;
        for bc=bcvec
            SVM = gqr_fitsvm(train_data,train_class,ep,bc,low_rank);%pause
            errvec(k) = sum(test_class ~= SVM.eval(test_data));
            marvec(k) = SVM.margin;
            svmvec(k) = sum(SVM.sv_index);
            fprintf('%d\t%d\t%4.2f\t%d\n',k,svmvec(k),bc,SVM.bias)
            k = k + 1;
        end
        
        h1 = figure;
        semilogx(bcvec,errvec,'linewidth',2)
        xlabel('C')
        ylabel('missed classifications')
        ylim([0,10])
        set(gca,'ytick',[0,5,10])
        
        h2 = figure;
        [AX,H1,H2] = plotyy(bcvec,svmvec,bcvec,marvec,'semilogx','loglog');
        xlabel('C')
        ylabel(AX(1),'support vectors')
        ylabel(AX(2),'margin')
        set(AX(1),'ycolor','k')
        set(AX(2),'ycolor','k')
        set(AX(1),'ylim',[0,200])
        set(AX(2),'ylim',[1e-5,10])
        set(AX(2),'xlim',[min(bcvec),max(bcvec)])
        set(AX(2),'xticklabel',{})
        set(AX(1),'ytick',[0,100,200])
        set(H1,'color','k')
        set(H2,'color','k')
        set(H1,'linestyle','--')
        set(H1,'linewidth',2)
        set(H2,'linewidth',2)
        title(sprintf('\\epsilon=%g',ep))
        legend('# SV','Margin','location','northeast')
    case 4
        % Loops over selected epsilon and box constraint values
        % and records the incorrect classifications for each
        errmat = zeros(length(epvec),length(bcvec));
        marmat = zeros(length(epvec),length(bcvec));
        svmmat = zeros(length(epvec),length(bcvec));
        kep = 1;
        h_waitbar = waitbar(0,'Initializing');
        for ep=epvec
            kbc = 1;
            for bc=bcvec
                SVM = gqr_fitsvm(train_data,train_class,ep,bc,low_rank);
                errmat(kep,kbc) = sum(test_class ~= SVM.eval(test_data));
                marmat(kep,kbc) = SVM.margin;
                svmmat(kep,kbc) = sum(SVM.sv_index);
                kbc = kbc + 1;
            end
            kep = kep + 1;
            progress = floor(100*kep/length(epvec))/100;
            waitbar(progress,h_waitbar,'Computing')
        end
        waitbar(1,h_waitbar,'Plotting')
        [E,B] = meshgrid(epvec,bcvec);
        
        h1 = figure;
        h_err = surf(E,B,errmat');
        set(h_err,'edgecolor','none')
        set(gca,'xscale','log')
        set(gca,'yscale','log')
        xlabel('\epsilon')
        ylabel('C')
        shading interp
        grid off
        
        h2 = figure;
        h_svm = surf(E,B,svmmat');
        set(h_svm,'edgecolor','none')
        set(gca,'xscale','log')
        set(gca,'yscale','log')
        xlabel('\epsilon')
        ylabel('C')
        shading interp
        grid off
        
        close(h_waitbar)
end