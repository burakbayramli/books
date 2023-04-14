function [alphaValues] ... 
            = shapeFunction2DQuad(N_1, N_2, N_3, N_4)
%% ------------------------------------------------------------------------
%% Shape Function script for 2D Triangular Higher Order elements
%% -------------------------------------------------------------------------
%Author:    Michael Okereke
%About:     Generates shape functions for 2D higher order triangular elements
%Date:      August 27th, 2016
% ------------------------------------------------------------------------
close all
clc

%% Node Coordinates
nodeCoordinates = [N_1; N_2; N_3; N_4];
matrixSize      = length(nodeCoordinates);

%% Determine the alpha coefficients of shape functions
%Initialize
    Ri  = cell(1);              Qi  = cell(1);
    coefR = cell(1);            coefQ = cell(1);
    N       = eye(matrixSize);  alphaValues = cell(1);
    Q       = cell(1);          R = cell(1); 
    Nvalues = cell(1);          funcN = cell(1);    p = cell(1);

 %% Iterate through all elements/nodal coordinates.
    for n = 1:4
      Nvalues{n} = N(:,n); 
      A = transpose(ones(matrixSize,1)*nodeCoordinates(:,1)');
      B = transpose(ones(matrixSize,1)*nodeCoordinates(:,2)');
      for ii = 1:length(A)
          Ri{ii} = A(:,ii);
          Qi{ii} = B(:,ii);
      end
      coefR{n} = cell2mat(Ri);
      coefQ{n} = cell2mat(Qi);
      Q{n} = [ones(4,1) coefR{n}(:,1) coefQ{n}(:,1) ...
                            coefR{n}(:,1).*coefQ{n}(:,1) Nvalues{n}];
      R{n} = rref(Q{n});
      alphaValues{n} = R{n}(:,end);

 %% Expression for N becomes
    xx = nodeCoordinates;
	aa     = alphaValues{n}(2)*xx(:,1); 
 	bb     = alphaValues{n}(3)*xx(:,2);
    dd     = alphaValues{n}(4)*xx(:,1).*xx(:,2);
	yy     = 1 + aa + bb + dd;

 %% Interpolated expression for Shape functions
        funcN{n} = yy;

%% Plot 3D Graphs of Shape Functions
%Declare parameters
    nodeCoordinatesSorted = sortrows(nodeCoordinates);
    dataSpread = min(nodeCoordinatesSorted(:,1)):...
        (max(nodeCoordinatesSorted(:,1))-min(nodeCoordinatesSorted(:,1)))/8:...
         max(nodeCoordinatesSorted(:,1));
    [A, B] = meshgrid(dataSpread, dataSpread);
    C      = alphaValues{n}(1) + alphaValues{n}(2).*A ...
                + alphaValues{n}(3).*B + alphaValues{n}(4).*A.*B;
 %% All plots on same figure
    figure(1)

        %Plot shape functions
        p{n} = surf(A,B,C, 'DisplayName',...
            ['N_',num2str(n), ' =  ', num2str(alphaValues{n}(1)),' + ',...
                    num2str(alphaValues{n}(2)),'\zeta + ',...
                    num2str(alphaValues{n}(3)),'\eta  + ',...
                    num2str(alphaValues{n}(4)),'\zeta^2']);
        hold all
          xlabel('Natural coordinate, \zeta','FontSize',18)
          ylabel('Natural coordinate, \eta','FontSize',18)
          zlabel('Shape Functions','FontSize',18)
          title('Plot of shape functions, N_i', 'FontSize',18)
          
         legend('show','location','best')
          
          box on
     
  %% Each plot on a different figure handle
         figure(n+1)
            axes1 = axes('Parent',figure(n+1),'LineWidth',1.5,'FontSize',30);
            view(axes1,[-37.5 30]);
            box(axes1,'on');
            grid(axes1,'off');
            hold(axes1,'all');
        %Plot shape functions
        p{n} = surf(A,B,C, 'Parent',axes1, 'DisplayName', ...
            ['N_',num2str(n), ' =  ', num2str(alphaValues{n}(1)),' + ',...
                    num2str(alphaValues{n}(2)),'\zeta + ',...
                    num2str(alphaValues{n}(3)),'\eta  + ',...
                    num2str(alphaValues{n}(4)),'\zeta^2']);
        colormap([0.800000011920929 0.800000011920929 0.800000011920929])
          hold all
          xlabel('\zeta','FontSize',30)
          ylabel('\eta','FontSize',30)
          zlabel('N','FontSize',30)
          title(['Plot of N_',num2str(n)],'FontSize',18)
          legend('show','Location','Best'); 
          box on
    end

%% Display alpha coefficients of the shape functions
    disp('Alpha Coefficients for [N1 N2, ..., Nn]  >>  ')
    disp(' ')
    disp(cell2mat(alphaValues));

end
%% **************************************************************