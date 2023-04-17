function [alphaValues] ... 
            = shapeFunction2DTri(N_1, N_2, N_3)
%% ------------------------------------------------------------------------
%% Shape Function script for 2D Triangular elements
%% -------------------------------------------------------------------------
%Author:    Michael Okereke
%About:     Generates shape functions for 1D elements
%Date:      August 19th, 2016
% ------------------------------------------------------------------------
close all
clc

%% Node Coordinates
nodeCoordinates = [N_1; N_2; N_3];
matrixSize      = length(nodeCoordinates);

%% Determine the alpha coefficients of shape functions
%Initialize
Ri  = cell(1);              coefR = cell(1);
N       = eye(matrixSize);  alphaValues = cell(1);
Q       = cell(1);          R = cell(1); 
Nvalues = cell(1);          funcN = cell(1);    p = cell(1);

%% Iterate through all elements/nodal coordinates.
for n = 1:3
  Nvalues{n} = N(:,n); 
  A = transpose(ones(matrixSize,1)*nodeCoordinates(:,1)');
  B = transpose(ones(matrixSize,1)*nodeCoordinates(:,2)');
  for ii = 1:length(A)
      Ri{ii} = A(:,ii);
      Qi{ii} = B(:,ii);
  end
  coefR{n} = cell2mat(Ri);
  coefQ{n} = cell2mat(Qi);
  Q{n} = [ones(3,1) coefR{n}(:,1) coefQ{n}(:,1) Nvalues{n}];
  R{n} = rref(Q{n});
  alphaValues{n} = R{n}(:,end);

%% Expression for N becomes
xx   	= nodeCoordinates;
aa      = alphaValues{n}(2)*xx(:,1);
bb      = alphaValues{n}(3)*xx(:,2);
yy      = alphaValues{1}(1) + aa + bb;

%% Interpolated expression for Shape functions
funcN{n} = yy;

%% Plot 3D Graphs of Shape Functions
%Declare parameters
nodeCoordinatesSorted = sortrows(nodeCoordinates);
dataSpread = min(nodeCoordinatesSorted(:,1)):...
    (max(nodeCoordinatesSorted(:,1))-min(nodeCoordinatesSorted(:,1)))/1:...
     max(nodeCoordinatesSorted(:,1));
[A, B] = meshgrid(dataSpread, dataSpread);
C      = alphaValues{n}(1) + alphaValues{n}(2).*A + alphaValues{n}(3).*B;
figure(1)
%Plot shape functions
p{n} = surf(A,B,C,'DisplayName',...
            ['N_',num2str(n), ' =  ', num2str(alphaValues{n}(1)),' + ',...
             num2str(alphaValues{n}(2)),'\zeta + ',...
             num2str(alphaValues{n}(3)),'\eta']);
hold all
xlabel('Natural coordinate, \zeta','FontSize',18)
ylabel('Natural coordinate, \eta','FontSize',18)
zlabel('Shape Functions','FontSize',18)
title('Plot of shape functions, N_i', 'FontSize',18)
      
legend('show','Location','Best');
box on
print -djpg /tmp/shape1.jpg
%% Each plot on a different figure handle
figure(n+1)
axes1 = axes('Parent',figure(n+1),'LineWidth',1.5,'FontSize',30);
view(axes1,[-37.5 30]);
box(axes1,'on');
grid(axes1,'on');
hold(axes1,'all');

    %Plot shape functions
p{n} = surf(A,B,C, 'Parent',axes1, 'DisplayName',...
            ['N_',num2str(n), ' =  ', num2str(alphaValues{n}(1)),' + ',...
             num2str(alphaValues{n}(2)),'\zeta + ',...
             num2str(alphaValues{n}(3)),'\eta']);
colormap gray
hold all
xlabel('\zeta','FontSize',30)
ylabel('\eta','FontSize',30)
zlabel('N','FontSize',30)
title(['Plot of N_',num2str(n)],'FontSize',30)
box on
print -djpg /tmp/shape2.jpg
      
end

%% Display alpha coefficients of the shape functions
disp('Alpha Coefficients for [N1 N2, ..., Nn]  >>  ')
disp(' ')
disp(cell2mat(alphaValues));

end
%% **************************************************************
