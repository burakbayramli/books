% Funtion to plot the aerodynamic panel model and results
% Anthony Ricciardi
% December 2014
%
% Input
% hand = [int>0] figure number
% FEM = model data structure
% Options = options data structure
%   Options.collocation = 'YES/NO'
%   Options.bound_cent = 'YES/NO'
%   Options.boxNums = 'YES/NO'
%   Options.n_vec = 'YES/NO'
%   Options.delta_pressures = 'YES/NO'
%   Options.Spanwise_Cl = 'YES/NO'
%   Options.CORD2R = 'YES/NO'
%   Options.forceVecs = [nBoxes,3] vector of forces on each box
% 
% Default = 'NO' for all Options
%
% Outputs
% Void.  Creates figure.
%
function [] = plotData(hand,FEM,Options)

if nargin < 3
    Options = [];
end

nb = size(FEM.boxes,2);

% calculate n_vecMult
if isfield(Options,'n_vec') || isfield(Options,'CORD2R')
n_vecMult = 0;
for i = 1:size(FEM.CAERO7,2)
    dummy = max(max(max(abs(FEM.CAERO7(i).panelPlaneCorners))));
    if dummy > n_vecMult
        n_vecMult = dummy;
    end
end
n_vecMult = n_vecMult/30;
end

%%
figure(hand)
hold on

% macro element plane
for i = 1:size(FEM.CAERO7,2)
    
    % % macro surfaces
    surf(squeeze(FEM.CAERO7(i).panelPlaneCorners(1,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(2,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD) ) 
    
    % % collocation points
    if isfield(Options,'collocation'); if strcmp(Options.collocation,'YES') || strcmp(Options.collocation,'yes') || strcmp(Options.collocation,'Yes')
            plot3(FEM.CAERO7(i).collocation(1,:),FEM.CAERO7(i).collocation(2,:),FEM.CAERO7(i).collocation(3,:),'r.')
    end; end

    % % CAERO7 Numbers
    if isfield(Options,'macroNums'); if strcmp(Options.macroNums,'YES') || strcmp(Options.macroNums,'yes') || strcmp(Options.macroNums,'Yes')
            text(...
            (FEM.CAERO7(i).XRL + FEM.CAERO7(i).XTL)/2 - FEM.CAERO7(i).RCH/3 ,...
            (FEM.CAERO7(i).YRL + FEM.CAERO7(i).YTL)/2,...
            (FEM.CAERO7(i).ZRL + FEM.CAERO7(i).ZTL)/2,...
            num2str(FEM.CAERO7(i).WID) );
    end; end

    % % force control points
    if isfield(Options,'bound_cent'); if strcmp(Options.bound_cent,'YES') || strcmp(Options.bound_cent,'yes') || strcmp(Options.bound_cent,'Yes')
            plot3(FEM.CAERO7(i).bound_cent(1,:),FEM.CAERO7(i).bound_cent(2,:),FEM.CAERO7(i).bound_cent(3,:),'b.')
    end; end


    % % normal Vectors
    if isfield(Options,'n_vec'); if strcmp(Options.n_vec,'YES') || strcmp(Options.n_vec,'yes') || strcmp(Options.n_vec,'Yes')
            iter = 0;
            for j = 1:FEM.CAERO7(i).NSPAN-1
                for k = 1:FEM.CAERO7(i).NCHORD-1
                    iter = iter + 1;
                    xyz = ([FEM.CAERO7(i).collocation(1,iter),FEM.CAERO7(i).collocation(2,iter),FEM.CAERO7(i).collocation(3,iter)]...
                        + [FEM.CAERO7(i).bound_cent(1,iter),FEM.CAERO7(i).bound_cent(2,iter),FEM.CAERO7(i).bound_cent(3,iter)])./2;
                    plot3([xyz(1),xyz(1)+n_vecMult*FEM.CAERO7(i).n_vec(1,iter)],[xyz(2),xyz(2)+n_vecMult*FEM.CAERO7(i).n_vec(2,iter)],[xyz(3),xyz(3)+n_vecMult*FEM.CAERO7(i).n_vec(3,iter)]);
                end
            end
    end; end

    % box numbers
    if isfield(Options,'boxNums'); if strcmp(Options.boxNums,'YES') || strcmp(Options.boxNums,'yes') || strcmp(Options.boxNums,'Yes')
    indx = find(FEM.boxes(2,:)==i);
    iter = 0;
    for j = 1:FEM.CAERO7(i).NSPAN-1
        for k = 1:FEM.CAERO7(i).NCHORD-1
            iter = iter + 1;
            xyz = ([FEM.CAERO7(i).collocation(1,iter),FEM.CAERO7(i).collocation(2,iter),FEM.CAERO7(i).collocation(3,iter)]...
                + [FEM.CAERO7(i).bound_cent(1,iter),FEM.CAERO7(i).bound_cent(2,iter),FEM.CAERO7(i).bound_cent(3,iter)])./2;
            text(xyz(1),xyz(2),xyz(3),num2str(FEM.boxes(1,indx(iter))));
        end
    end
    end; end
    
    
end

% force vectors
if isfield(Options,'forceVecs');
fMult = n_vecMult/ max(sqrt(Options.forceVecs(:,1).^2+Options.forceVecs(:,2).^2+Options.forceVecs(:,3).^2));
for b1 = 1:nb
    b1i = FEM.boxes(2,b1); % macro element number 
    b1j = FEM.boxes(3,b1); % box in macro element
    plot3([FEM.CAERO7(b1i).bound_cent(1,b1j),FEM.CAERO7(b1i).bound_cent(1,b1j)+fMult*Options.forceVecs(b1,1)],...
        [FEM.CAERO7(b1i).bound_cent(2,b1j),FEM.CAERO7(b1i).bound_cent(2,b1j)+fMult*Options.forceVecs(b1,2)],...
        [FEM.CAERO7(b1i).bound_cent(3,b1j),FEM.CAERO7(b1i).bound_cent(3,b1j)+fMult*Options.forceVecs(b1,3)],'k-')
end
end


% coordinate systems
if isfield(Options,'CORD2R'); if strcmp(Options.CORD2R,'YES') || strcmp(Options.CORD2R,'yes') || strcmp(Options.CORD2R,'Yes')
if isfield(FEM,'CORD2R')
for i = 1:size(FEM.CORD2R,2)
    A = [FEM.CORD2R(i).A1,FEM.CORD2R(i).A2,FEM.CORD2R(i).A3];
    B = [FEM.CORD2R(i).B1,FEM.CORD2R(i).B2,FEM.CORD2R(i).B3];
    C = [FEM.CORD2R(i).C1,FEM.CORD2R(i).C2,FEM.CORD2R(i).C3];
    z = (B-A)./norm(B-A);
    nu = (C-A);
    y = cross(z,nu); y = y./norm(y);
    x = cross(y,z); x = x./norm(x);
    
%     TT(i,:) = A;
%     CC(:,:,i) = [x;y;z];
    
    plot3([A(1),A(1)+2*n_vecMult*x(1)],[A(2),A(2)+2*n_vecMult*x(2)],[A(3),A(3)+2*n_vecMult*x(3)],'k-','linewidth',3)
    plot3([A(1),A(1)+2*n_vecMult*y(1)],[A(2),A(2)+2*n_vecMult*y(2)],[A(3),A(3)+2*n_vecMult*y(3)],'k-','linewidth',3)
    plot3([A(1),A(1)+2*n_vecMult*z(1)],[A(2),A(2)+2*n_vecMult*z(2)],[A(3),A(3)+2*n_vecMult*z(3)],'k-','linewidth',3)
    text(A(1)+2.5*n_vecMult*x(1),A(2)+2.5*n_vecMult*x(2),A(3)+2.5*n_vecMult*x(3),'x')
    text(A(1)+2.5*n_vecMult*y(1),A(2)+2.5*n_vecMult*y(2),A(3)+2.5*n_vecMult*y(3),'y')
    text(A(1)+2.5*n_vecMult*z(1),A(2)+2.5*n_vecMult*z(2),A(3)+2.5*n_vecMult*z(3),'z')
    
    text(A(1)+.25*n_vecMult*(x(1)+y(1)+z(1)),A(2)+.25*n_vecMult*(x(2)+y(2)+z(2)),A(3)+.25*n_vecMult*(x(3)+y(3)+z(3)),num2str(FEM.CORD2R(i).CID))
end
end
end; end

hold off
axis equal

%% Plot delta pressures
if isfield(Options,'delta_pressures'); if strcmp(Options.delta_pressures,'YES') || strcmp(Options.delta_pressures,'yes') || strcmp(Options.delta_pressures,'Yes')

figure(hand + 10)
hold on
for i = 1:size(FEM.CAERO7,2)
    dp = squeeze( FEM.CAERO7(i).forceGrid./FEM.CAERO7(i).areaGrid)./FEM.TRIM.QINF;
    
    % check pressure vector orientation - needs adjustment for single
    % box length or width surfaces.
    if size(dp,1)==1 || size(dp,2)==1
        dp = dp';
    end
    surf(squeeze(FEM.CAERO7(i).panelPlaneCorners(1,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(2,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(3,:,:)), dp)
end
hold off
axis equal
colorbar
title('\Delta p / q_\infty')
    
end; end

%% Plot Spanwise Lift
if isfield(Options,'Spanwise_Cl'); if strcmp(Options.Spanwise_Cl,'YES') || strcmp(Options.Spanwise_Cl,'yes') || strcmp(Options.Spanwise_Cl,'Yes')

        figure(hand + 20)
        hold on
        for i = 1:size(FEM.CAERO7,2)
            plot(FEM.CAERO7(i).SpanwiseLocation(:,2),FEM.CAERO7(i).Spanwise_Cl)
        end
        hold off
        
end; end
% plot(FEM.CAERO7(i).SpanwiseLocation(:,2),FEM.CAERO7(i).Spanwise_Cd)


%% Plot box areas (test)
% figure(4)
% hold on
% for i = 1:size(FEM.CAERO7,2)
%     area = squeeze(FEM.CAERO7(i).areaGrid);
%     if size(area,2)==1
%         area = area';
%     end
%     surf(squeeze(FEM.CAERO7(i).panelPlaneCorners(1,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(2,:,:)),squeeze(FEM.CAERO7(i).panelPlaneCorners(3,:,:)), area )
% end
% hold off
% colorbar


%% camber surface
%     surf(squeeze(cambered_panels(1,:,:)),squeeze(cambered_panels(2,:,:)),squeeze(cambered_panels(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1) )
%     plot3(FEM.CAERO7(i).cambered_collocation(1,:),FEM.CAERO7(i).cambered_collocation(2,:),FEM.CAERO7(i).cambered_collocation(3,:),'k.')
%     plot3(FEM.CAERO7(i).cambered_te_in(1,:),FEM.CAERO7(i).cambered_te_in(2,:),FEM.CAERO7(i).cambered_te_in(3,:),'r.')
%     plot3(FEM.CAERO7(i).cambered_te_out(1,:),FEM.CAERO7(i).cambered_te_out(2,:),FEM.CAERO7(i).cambered_te_out(3,:),'ro')
% 
%     % plot normals
%     for j = 1:newBoxes
%             plot3([FEM.CAERO7(i).cambered_collocation(1,j),FEM.CAERO7(i).cambered_collocation(1,j)+FEM.CAERO7(i).n_vec(1,j)],[FEM.CAERO7(i).cambered_collocation(2,j),FEM.CAERO7(i).cambered_collocation(2,j)+FEM.CAERO7(i).n_vec(2,j)],[FEM.CAERO7(i).cambered_collocation(3,j),FEM.CAERO7(i).cambered_collocation(3,j)+FEM.CAERO7(i).n_vec(3,j)],'k-')
%     end

%% compare to VoLAre1
% axis equal
% xlabel('x'); ylabel('y'); zlabel('z');
% hold off
% view(-25,30)
% 
% load wV151.mat wV1
% wDiff = 100*abs(wV1 - w)./abs(wV1);
% % wDiff(1:10,1:10)
% max(max(abs(wDiff)))



%     clf
%     hold on
%     surf(squeeze(panelPlaneCorners(1,:,:)),squeeze(panelPlaneCorners(2,:,:)),squeeze(panelPlaneCorners(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD) ) %,'FaceColor','interp','FaceAlpha',0.5,'EdgeColor',[1,0,0],'FaceLighting','phong','AmbientStrength',0.5)
%     mesh(squeeze(bound_vortex(1,:,:)),squeeze(bound_vortex(2,:,:)),squeeze(bound_vortex(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1) ) %,'FaceColor','interp','FaceAlpha',0.5,'EdgeColor',[1,0,0],'FaceLighting','phong','AmbientStrength',0.5)
% plot3(FEM.CAERO7(i).bound_cent(1,:),FEM.CAERO7(i).bound_cent(2,:),FEM.CAERO7(i).bound_cent(3,:),'k.')
%     plot3(FEM.CAERO7(i).collocation(1,:),FEM.CAERO7(i).collocation(2,:),FEM.CAERO7(i).collocation(3,:),'k.')
%
% plot3(FEM.CAERO7(i).cambered_te_in(1,:),FEM.CAERO7(i).cambered_te_in(2,:),FEM.CAERO7(i).cambered_te_in(3,:),'r.')
% plot3(FEM.CAERO7(i).cambered_te_out(1,:),FEM.CAERO7(i).cambered_te_out(2,:),FEM.CAERO7(i).cambered_te_out(3,:),'b.')

% plot3(tipCamber(1,:),tipCamber(2,:),tipCamber(3,:),'k.')
% plot3(rootCamber(1,:),rootCamber(2,:),rootCamber(3,:),'k.')
% plot3(midCamber(1,:),midCamber(2,:),midCamber(3,:),'k.')
% plot3(FEM.CAERO7(i).te_in(1,:),FEM.CAERO7(i).te_in(2,:),zeros(size(FEM.CAERO7(i).te_in(2,:))))

% mesh(squeeze(control_lattice(1,:,:)),squeeze(control_lattice(2,:,:)),squeeze(control_lattice(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1) )
% mesh(squeeze(cambered_bound_vortex_lattice(1,:,:)),squeeze(cambered_bound_vortex_lattice(2,:,:)),squeeze(cambered_bound_vortex_lattice(3,:,:)),.25*ones(FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1) )
