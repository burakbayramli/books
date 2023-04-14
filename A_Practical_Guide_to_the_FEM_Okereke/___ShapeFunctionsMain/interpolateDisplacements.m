function [uNI, xx, funcN] = interpolateDisplacements(u,xStart, xEnd)
%% Interpolated displacements
%Author:  Michael Okereke
%Date:    August 10th, 2016
%-------------------------------------------------------------
close all

%Determine the shape functions expressions
    [~, xx, funcN] = shapeFunction1D(length(u)-1, xStart, xEnd);

% Determine interpolated displacements
for uIndex = 1:length(u)
    %Find interpolated displacements
    uNI = zeros(length(xx),1);
    for zz = 1:length(funcN)
         NI = funcN{zz}*u(zz);
        uNI = uNI + NI;
    end
end

%% Plot of interpolated displacements
     figure(2)
     %Current Legend Texts - Transposed;
        %a = legh.String';
     %Plot graphs now
     plot(xx, uNI, '-','DisplayName','Interpolated displacement')
     hold all
     xlabel('Nodal positions')
     ylabel('Interpolated displacement')
     title('Plot of interpolated displacements')
      
     %Plot the original nodal displacements
        plot(xStart:(xEnd-xStart)/(length(u)-1):xEnd, u, 's','markersize',10, 'DisplayName','Original Displacement')
        legend('show', 'location','best');
end
