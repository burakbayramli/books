%% Determine the Profile of the structure both in deformed and undeformed
%% configuration (plotted on one axis).
%Date:      18th July, 2016
%Author:    Dr. Michael I. Okereke
%About:     This Matlab script is plotting the deformed profile of the
%           structure after the FE solver has determined its nodal displacement and
%           forces. 

% Associate x- and y-node numbers
    u_x                 =   1:2:2*numberNodes - 1;
    u_y                 =   2:2:2*numberNodes;

%Initial edge length
    L                   =  xx(2) - xx(1);
    
%X- and Y-displacement values
    XX                  =   displacement(u_x);
    YY                  =   displacement(u_y);
    
%% Create the Scale Factor for the displacement
    %scaleMultiplier     =   6000;
    dispNorm            =   max(sqrt(XX.^2 + YY.^2)); %Find the maximum distance in the plot
    scaleFact           =   scaleMultiplier*dispNorm; %Scale this distance by the Scale multiplier

 %Deformed Nodal coordinates
%     defNodeCoordinates  =   nodeCoordinates + scaleFact*[XX YY];
    defNodeCoordinates  =   nodeCoordinates + scaleMultiplier*[XX YY];
    
%% Draw the Deformed Profiles
close all
figure
for e = 1:numberElements;
    %elementDof:    element degrees of freedom (Dof)
    indice              =   elementNodes(e,:);
    undeformNodes       =   [xx(indice(1)) yy(indice(1)); xx(indice(2)) yy(indice(2))];
    deformNodes         =   [xx(indice(1)) + scaleMultiplier*XX(indice(1)) yy(indice(1)) + scaleMultiplier*YY(indice(1));
                             xx(indice(2)) + scaleMultiplier*XX(indice(2)) yy(indice(2)) + scaleMultiplier*YY(indice(2))] ;
    hold on
    plot(undeformNodes(:,1), undeformNodes(:,2), 'ks--', 'LineWidth',1.5, 'markersize',10, 'MarkerFaceColor',[1 1 1])
    plot(deformNodes(:,1)  , deformNodes(:,2),   'ro-',  'LineWidth',1.5, 'markersize',10, 'MarkerFaceColor',[1 1 1])
end
if scaleMultiplier == 1
    title('Deformed profile of structure (drawn to scale)')
else
    title(['Deformed profile of structure with displacement scaled < ', num2str(scaleMultiplier), ' > times.'])
end
    xlabel('Global X-axis')
    ylabel('Global Y-axis')
    legend('Undeformed', 'Deformed','location','best')
    box(gca,'on');

%Save Figure
saveas(gcf,[jobFolder,'/Job',num2str(counter),'_DeformedProfile'],'fig')

 %% Close all open files
    fclose all;