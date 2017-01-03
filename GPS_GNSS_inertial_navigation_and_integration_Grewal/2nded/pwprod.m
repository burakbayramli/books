% pwprod.m
%
% Demonstration of pointwise products of Gaussian probability densities
% and Gaussian likelihood functions, showing that the product functions
% are "Gaussian-like," and calculating the theoretical parameters of
% the resulting aleged Gaussian density functions.
%
% Calculates the information matrices and means of the pointwise products.
%
% Displays the means in the title on the pointwise product plots.
%
% Displayed mean should be at the peak value of the pointwise product.
%
% Generates six plots:
%    1. Gaussian likelihood function with mean [-2;2]
%       and information matrix [1/4,0;0,1], for which
%       covariance matrix would be [4,0;0,1].
%    2. Gaussian likelihood function with mean [2;-2]
%       and information matrix [1,0;0,1/4], for which
%       covariance matrix would be [1,0;0,4].
%    3. Pointwise product of (1) and (2), which should
%       have information matrix
%       [1/4,0;0,1]+[1,0;0,1/4] = [5/4,0;0,5/4]
%       and mean
%       [5/4,0;0,5/4]\([1/4,0;0,1]*[-2;2]+[1,0;0,1/4]*[2;-2])
%    4. Gaussian likelihood function with mean [-2;2]
%       and information matrix [1,.97;.97,1], for which covariance
%       matrix would be [16.9205,-16.4129;-16.4129,16.9205] and
%       correlation coefficient would be -0.97.
%       The distribution should be elongate in the [1,-1]-direction.
%    5. Gaussian likelihood function with mean [2;-2]
%       and information matrix [1,-1;-1,1].  THIS IS A DEGENERATE
%       INFORMATION MATRIX, WITH RANK = 1.  IT HAS NO COVARIANCE
%       MATRIX.  All values in the [1,1] direction should be equally
%       likely, and the plot should show a ridge of constant height
%       in that direction.
%    6. Pointwise product of (1) and (2), which should
%       have information matrix
%       [1,.97;.97,1]+[1,-1;-1,1] = [2,-0.03;-0.03,2]
%       and mean
%       [2,-0.03;-0.03,2]\([1,.97;.97,1]*[-2;2]+[1,-1;-1,1]*[2;-2])
% Plots are in left-handed matrix plot coordinates.
%
clear all
close all
x=(-8:.5:8);
y=x;
%
% First Gaussian likelihood function parameters
%
mu1 = [-2;2];      % mean
Y1  = [1/4,0;0,1]; % information matrix
%
% Second Gaussian likelihood function parameters
%
mu2 = [2;-2];      % mean
Y2  = [1,0;0,1/4];
%
% Third Gaussian likelihood function parameters
% CALCULATED, BUT NOT USED
%
Y3  = Y1+Y2;              % information matrix
mu3 = Y3\(Y1*mu1+Y2*mu2); % mean
%
% Fourth Gaussian likelihood function parameters
%
mu4 = [-2;2];        % mean
Y4  = [1,.97;.97,1]; % information matrix
%
% Fifth Gaussian likelihood function parameters
%
mu5 = [2;-2];      % mean
Y5  = [1,-1;-1,1]; % information matrix
%
% Sixth Gaussian likelihood function parameters
% CALCULATED, BUT NOT USED
%
Y6  = Y4+Y5;              % information matrix
mu6 = Y6\(Y4*mu4+Y5*mu5); % mean
%
for i=1:33,
   for j=1:33,
      H1(i,j) = exp(-[x(i)-mu1(1);y(j)-mu1(2)]'*Y1*[x(i)-mu1(1);y(j)-mu1(2)]/2);
      H2(i,j) = exp(-[x(i)-mu2(1);y(j)-mu2(2)]'*Y2*[x(i)-mu2(1);y(j)-mu2(2)]/2);
      H4(i,j) = exp(-[x(i)-mu4(1);y(j)-mu4(2)]'*Y4*[x(i)-mu4(1);y(j)-mu4(2)]/2);
      H5(i,j) = exp(-[x(i)-mu5(1);y(j)-mu5(2)]'*Y5*[x(i)-mu5(1);y(j)-mu5(2)]/2);
   end;
end;
figure;
mesh(x,y,H1);
title('1st Gaussian Distribution');
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Relative Density');
figure;
mesh(x,y,H2);
title('2nd Gaussian Distribution');
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Relative Density');
figure;
mesh(x,y,H1.*H2/max(max(H1.*H2))); % pointwise product
title(['Product of 1st & 2nd Distributions: Peak at (',num2str(mu3(1)),',',num2str(mu3(2)),')']);
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Product of Rel. Densities');
figure;
mesh(x,y,H4);
title('4th Gasussian Likelihood Function (Correlation Coefficient = -0.97)');
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Relative Likelihood');
figure;
mesh(x,y,H5);
title('5th Gaussian Likelihood Function (Singular Information Matrix)');
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Relative Likelihood');
figure;
mesh(x,y,H4.*H5/max(max(H4.*H5))); % pointwise product
title(['Product of 4th & 5th Likelihoods: Peak at (',num2str(mu6(1)),',',num2str(mu6(2)),')']);
xlabel('Y-Axis');
ylabel('X-Axis');
zlabel('Relative Likelihood');
