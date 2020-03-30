% This tests the simple SVM results in the book.
% Only the basic plots of the input data and a few contours are provided
% here.  h1, h2, and h3 are the associated figure handles.

% To allow for the low-rank expansion parameter to be set
global GAUSSQR_PARAMETERS

% Use the low rank matrix multiplication strategy
low_rank = 0;
GAUSSQR_PARAMETERS.DEFAULT_REGRESSION_FUNC = .05;

% Define the size of the problem
test_N = 10;
train_N = 100;

% Create random training and test data
[train_data,train_class,test_data,test_class,h1] = SVM_setup(1,train_N,test_N);

% Plot a variety of contours
d = 0.02;
[CD1,CD2] = meshgrid(min(train_data(:,1)):d:max(train_data(:,1)),...
    min(train_data(:,2)):d:max(train_data(:,2)));
contour_data_fine = [CD1(:),CD2(:)];
CD3 = CD1(1:3:end,1:3:end);
CD4 = CD2(1:3:end,1:3:end);
contour_data_markers = [CD3(:),CD4(:)];

% First, fix the box constraint and consider 3 ep values
bc = 1;
epvec = [.2 1 5];
ptvec = {'o','*','none'};
h2 = figure;
hold on
h_contour = zeros(size(epvec));
for k=1:length(epvec)
    SVM = gqr_fitsvm(train_data,train_class,epvec(k),bc,low_rank);
    contour_class = SVM.eval(contour_data_fine);
    contour(CD1,CD2,reshape(contour_class,size(CD1)),[0 0],'linewidth',2);
    contour_class = SVM.eval(contour_data_markers);
    [tmp,h_contour(k)] = contour(CD3,CD4,reshape(contour_class,size(CD3)),[0 0]);
    set(get(h_contour(k),'Children'),'Marker',ptvec{k});
    set(get(h_contour(k),'Children'),'LineStyle','none');
end
title(sprintf('C=%g',bc));
%set(h2,'renderer','zbuffer');
h_legend = legend(h_contour,'\epsilon=.2','\epsilon=1','\epsilon=5','location','southeast');
c=get(h_legend,'Children');
% I don't really know what I'm doing here, but it works
%for k=1:length(epvec)
%    set(get(get(c(2*k-1),'Children'),'Children'),'Marker',ptvec{length(epvec)-k+1});
%    set(get(get(c(2*k-1),'Children'),'Children'),'linewidth',2);
%end
hold off

% First, fix the box constraint and consider 3 ep values
ep = 1;
bcvec = [1e-2 1e0 1e5];
ptvec = {'o','*','none'};
h3 = figure;
hold on
h_contour = zeros(size(epvec));
for k=1:length(epvec)
    SVM = gqr_fitsvm(train_data,train_class,ep,bcvec(k),low_rank);
    contour_class = SVM.eval(contour_data_fine);
    contour(CD1,CD2,reshape(contour_class,size(CD1)),[0 0],'linewidth',2);
    contour_class = SVM.eval(contour_data_markers);
    [tmp,h_contour(k)] = contour(CD3,CD4,reshape(contour_class,size(CD3)),[0 0]);
    set(get(h_contour(k),'Children'),'Marker',ptvec{k});
    set(get(h_contour(k),'Children'),'LineStyle','none');
end
title(sprintf('\\epsilon=%g',ep));
set(h3,'renderer','zbuffer'); % Not sure if this is necessary
h_legend = legend(h_contour,'C=.01','C=1','C=10000','location','southeast');
c=get(h_legend,'Children');
% I don't really know what I'm doing here, but it works
% for k=1:length(epvec)
%     set(get(get(c(2*k-1),'Children'),'Children'),'Marker',ptvec{length(bcvec)-k+1});
%     set(get(get(c(2*k-1),'Children'),'Children'),'linewidth',2);
% end
hold off