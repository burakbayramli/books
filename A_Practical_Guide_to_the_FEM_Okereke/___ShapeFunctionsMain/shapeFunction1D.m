function [alphaValues, xx, funcN] ... 
            = shapeFunction1D(numberElements, xStart, xEnd)
%% ------------------------------------------------------------------------
%% Shape Function script for 1D elements
%% -------------------------------------------------------------------------
%Author:    Michael Okereke
%About:     Generates shape functions for 1D elements
%Date:      August 8th, 2016
% ------------------------------------------------------------------------
close all

%% Describe nodal coordinates
    nodeCoordinates = xStart:(xEnd - xStart)/numberElements:xEnd;
    matrixSize = length(nodeCoordinates);

%% Determine the alpha coefficients of shape functions
%Initialize
    Ri = cell(1); coefR = cell(1);
    N    = eye(matrixSize); alphaValues = cell(1);
    Q   = cell(1); R = cell(1); 
    Nvalues = cell(1); funcN = cell(1);

 %% Iterate through all elements/nodal coordinates.
    for n = 1:matrixSize
      Nvalues{n} = N(:,n); 
      A = transpose(ones(matrixSize,1)*nodeCoordinates);
      for ii = 1:length(A)
          Ri{ii} = A(:,ii).^(ii-1);
      end
      coefR{n} = cell2mat(Ri);
      Q{n} = [coefR{n} Nvalues{n}];
      R{n} = rref(Q{n});
      alphaValues{n} = R{n}(:,end);

 %% Expression for N becomes
      xx = xStart:(xEnd - xStart)/100:xEnd;
      yy = zeros(length(xx),1);
      for jj = 1:length(alphaValues{n})
         aa     = alphaValues{n}(jj)*xx.^(jj-1)';
         yy     = yy + aa;
      end

 %% Interpolated expression for Shape functions
        funcN{n} = yy;

%% Plot Graphs of Shape Functions
        figure(1)
        %Plot shape functions
        plot(xx(1,:)', yy(:,1), '-', 'DisplayName',['N_',num2str(n)])
            hold all
        xlabel('Isoparametric natural coordinate, \zeta')
        ylabel('Shape Functions')

        legend('show','location','best')
          
    end

%% Display alpha coefficients of the shape functions
    disp('Alpha Coefficients for [N1 N2, ..., Nn]  >>  ')
    disp(' ')
    disp(cell2mat(alphaValues));


end
%% **************************************************************