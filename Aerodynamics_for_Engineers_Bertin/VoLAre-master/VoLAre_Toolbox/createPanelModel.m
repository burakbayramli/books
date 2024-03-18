% Function to create panel model from input data
%
% Anthony Ricciardi
% December 2014
%
% Inputs
% FEM = [struct] model data
%
% Outputs
% FEM = [struct] model data (Modified)
%
function FEM = createPanelModel(FEM)

%% Extract Numbers
if isfield(FEM,'AEFACT')
    dum = struct2cell(FEM.AEFACT);
    FEM.LIST.AEFACT.SID = squeeze(cell2mat(dum(1,1,:)));
end

if isfield(FEM,'PAFOIL7')
    dum = struct2cell(FEM.PAFOIL7);
    FEM.LIST.PAFOIL7.ID = squeeze(cell2mat(dum(1,1,:)));
end
clear dum

%% Extract Model
boxCount = 0;
FEM.boxes = [];
FEM.boxBoundVtxLength = [];
FEM.boxBoundVtxUnitVec = [];
FEM.boxQuarterPanelPoint = [];

for i = 1:size(FEM.CAERO7,2)
    
    if FEM.CAERO7(i).ACOORD~=0
        acoordIndex = find([FEM.ACOORD.ID]==FEM.CAERO7(i).ACOORD);
        dXYZ = [FEM.ACOORD(acoordIndex).XORIGN,FEM.ACOORD(acoordIndex).YORIGN,FEM.ACOORD(acoordIndex).ZORIGN];
        if FEM.ACOORD(acoordIndex).DELTA~=0 || FEM.ACOORD(acoordIndex).THETA~=0
            error('ACOORD ID %d has a nonzero DELTA or THETA field. This is unsupported.',FEM.ID(acoordIndex))
        end
    else
        dXYZ = [0,0,0];
    end
        
    % root and tip leading edges
    RL = [FEM.CAERO7(i).XRL,FEM.CAERO7(i).YRL,FEM.CAERO7(i).ZRL]+dXYZ;
    TL = [FEM.CAERO7(i).XTL,FEM.CAERO7(i).YTL,FEM.CAERO7(i).ZTL]+dXYZ;
       
    % exterior macro element points
    macroPoints = [RL; TL; TL + [FEM.CAERO7(i).TCH 0 0]; RL + [FEM.CAERO7(i).RCH 0 0]; RL ]';
    
    % spanwise points
    if FEM.CAERO7(i).LSPAN == 0
        spanPoints = linspace(0,1,FEM.CAERO7(i).NSPAN);
    else
        aefact_ind = find(FEM.LIST.AEFACT.SID == FEM.CAERO7(i).LSPAN);
        spanPoints = ( FEM.AEFACT(aefact_ind).VALUE ) ./ 100;
    end
    
    
    leSpanPoints =[ macroPoints(1,1) + (macroPoints(1,2)-macroPoints(1,1))*spanPoints;
        macroPoints(2,1) + (macroPoints(2,2)-macroPoints(2,1))*spanPoints;
        macroPoints(3,1) + (macroPoints(3,2)-macroPoints(3,1))*spanPoints];
    teSpanPoints =[ macroPoints(1,4) + (macroPoints(1,3)-macroPoints(1,4))*spanPoints;
        macroPoints(2,4) + (macroPoints(2,3)-macroPoints(2,4))*spanPoints;
        macroPoints(3,4) + (macroPoints(3,3)-macroPoints(3,4))*spanPoints];
    
    leSpanControlPoints = (leSpanPoints(:,2:FEM.CAERO7(i).NSPAN)+leSpanPoints(:,1:FEM.CAERO7(i).NSPAN-1) )./2;
    
    
    % chordwise points
    if FEM.CAERO7(i).LRCHD == 0
        lrChordPoints = linspace(0,1,FEM.CAERO7(i).NCHORD);
    else
        aefact_ind = find(FEM.LIST.AEFACT.SID == FEM.CAERO7(i).LRCHD);
        lrChordPoints = ( FEM.AEFACT(aefact_ind).VALUE ) ./ 100;
    end
    if FEM.CAERO7(i).LTCHD == 0
        ltChordPoints = linspace(0,1,FEM.CAERO7(i).NCHORD); 
    else
        aefact_ind = find(FEM.LIST.AEFACT.SID == FEM.CAERO7(i).LTCHD);
        ltChordPoints = ( FEM.AEFACT(aefact_ind).VALUE ) ./ 100;
    end
    
    rootChordPoints =[ macroPoints(1,1) + (macroPoints(1,4)-macroPoints(1,1))*lrChordPoints;
        repmat(macroPoints(2,1),[1,FEM.CAERO7(i).NCHORD]);
        repmat(macroPoints(3,1),[1,FEM.CAERO7(i).NCHORD])];

    tipChordPoints =[ macroPoints(1,2) + (macroPoints(1,3)-macroPoints(1,2))*ltChordPoints;
        repmat(macroPoints(2,2),[1,FEM.CAERO7(i).NCHORD]);
        repmat(macroPoints(3,2),[1,FEM.CAERO7(i).NCHORD])];
    
    % boundary and force control points
    rootControl = rootChordPoints(:,1:FEM.CAERO7(i).NCHORD-1);
    rootForce = rootControl;
    rootForce(1,:) = rootForce(1,:) + diff(rootChordPoints(1,:))./4;
    rootControl(1,:) = rootControl(1,:) + 3*diff(rootChordPoints(1,:))./4;
    
    tipControl = tipChordPoints(:,1:FEM.CAERO7(i).NCHORD-1);
    tipForce = tipControl;
    tipForce(1,:) = tipForce(1,:) + diff(tipChordPoints(1,:))./4;
    tipControl(1,:) = tipControl(1,:) + 3*diff(tipChordPoints(1,:))./4;
    
    % check panel direction
    le_direction = tipChordPoints(:,1)-rootChordPoints(:,1);
    if abs(le_direction(2)) < 1e-5
        nvecPlane = [0;0;1];
        thick_normal = [0;1;0];
    else
        nvecPlane = [0;1;0];
        thick_normal = [0;0;1];
    end
    
    % panel plane corners
    panelPlaneCorners = zeros(3,FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD);
    for j = 1:FEM.CAERO7(i).NCHORD
        panelPlaneCorners(:,:,j)=PlaneIntersectLine(leSpanPoints,nvecPlane,rootChordPoints(:,j),tipChordPoints(:,j)-rootChordPoints(:,j));
    end
    
    % bound vortex locations
    bound_vortex = zeros(3,FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1);
    for j = 1:FEM.CAERO7(i).NCHORD - 1
        bound_vortex(:,:,j)=PlaneIntersectLine(leSpanPoints,nvecPlane,rootForce(:,j),tipForce(:,j)-rootForce(:,j));
    end
    
    % control point lattice
    control_lattice = zeros(3,FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1);
    for j = 1:FEM.CAERO7(i).NCHORD - 1
        control_lattice(:,:,j)=PlaneIntersectLine(leSpanPoints,nvecPlane,rootControl(:,j),tipControl(:,j)-rootControl(:,j));
    end
    
    %% Panels in macro element plane    
    % panel corners
    FEM.CAERO7(i).le_in = [];
    FEM.CAERO7(i).te_in = [];
    FEM.CAERO7(i).le_out = [];
    FEM.CAERO7(i).te_out = [];
    
    % bound vortex points
    FEM.CAERO7(i).bound_in = [];
    FEM.CAERO7(i).bound_out = [];
    FEM.CAERO7(i).bound_cent = [];
    
    % control points
    FEM.CAERO7(i).collocation = [];
    
    % tailing vortex points
    FEM.CAERO7(i).trail_in_te = [];
    FEM.CAERO7(i).trail_out_te = [];
       
    for j = 1:FEM.CAERO7(i).NSPAN-1
        FEM.CAERO7(i).le_in  = [FEM.CAERO7(i).le_in,  squeeze(panelPlaneCorners(:,j  ,1:FEM.CAERO7(i).NCHORD-1)) ];
        FEM.CAERO7(i).te_in  = [FEM.CAERO7(i).te_in,  squeeze(panelPlaneCorners(:,j  ,2:FEM.CAERO7(i).NCHORD  )) ];
        FEM.CAERO7(i).le_out = [FEM.CAERO7(i).le_out, squeeze(panelPlaneCorners(:,j+1,1:FEM.CAERO7(i).NCHORD-1)) ];
        FEM.CAERO7(i).te_out = [FEM.CAERO7(i).te_out, squeeze(panelPlaneCorners(:,j+1,2:FEM.CAERO7(i).NCHORD  )) ];
        
        FEM.CAERO7(i).bound_in  = [FEM.CAERO7(i).bound_in,  squeeze(bound_vortex(:,j  ,1:FEM.CAERO7(i).NCHORD-1)) ];
        FEM.CAERO7(i).bound_out = [FEM.CAERO7(i).bound_out, squeeze(bound_vortex(:,j+1,1:FEM.CAERO7(i).NCHORD-1)) ];
        FEM.CAERO7(i).bound_cent = [FEM.CAERO7(i).bound_cent, (squeeze(bound_vortex(:,j  ,1:FEM.CAERO7(i).NCHORD-1)) + squeeze(bound_vortex(:,j+1,1:FEM.CAERO7(i).NCHORD-1)) )./2];
        
        FEM.CAERO7(i).collocation = [FEM.CAERO7(i).collocation, (squeeze(control_lattice(:,j  ,1:FEM.CAERO7(i).NCHORD-1)) + squeeze(control_lattice(:,j+1,1:FEM.CAERO7(i).NCHORD-1)) )./2];
        
        FEM.CAERO7(i).trail_in_te  = [FEM.CAERO7(i).trail_in_te,  repmat(panelPlaneCorners(:,j  ,FEM.CAERO7(i).NCHORD),[1,FEM.CAERO7(i).NCHORD-1]) ];
        FEM.CAERO7(i).trail_out_te = [FEM.CAERO7(i).trail_out_te, repmat(panelPlaneCorners(:,j+1,FEM.CAERO7(i).NCHORD),[1,FEM.CAERO7(i).NCHORD-1]) ];

    end
    
    a1 = cross( (panelPlaneCorners(:,1:FEM.CAERO7(i).NSPAN-1,2:FEM.CAERO7(i).NCHORD) - panelPlaneCorners(:,1:FEM.CAERO7(i).NSPAN-1,1:FEM.CAERO7(i).NCHORD-1))   , (panelPlaneCorners(:,2:FEM.CAERO7(i).NSPAN,1:FEM.CAERO7(i).NCHORD-1) - panelPlaneCorners(:,1:FEM.CAERO7(i).NSPAN-1,1:FEM.CAERO7(i).NCHORD-1) ));
    a2 = cross( (panelPlaneCorners(:,2:FEM.CAERO7(i).NSPAN,1:FEM.CAERO7(i).NCHORD-1) - panelPlaneCorners(:,2:FEM.CAERO7(i).NSPAN,2:FEM.CAERO7(i).NCHORD  )) , (panelPlaneCorners(:,1:FEM.CAERO7(i).NSPAN-1,2:FEM.CAERO7(i).NCHORD  ) - panelPlaneCorners(:,2:FEM.CAERO7(i).NSPAN,2:FEM.CAERO7(i).NCHORD  )));
    FEM.CAERO7(i).areaGrid = .5*( sqrt(sum(a1.^2)) + sqrt(sum(a2.^2)) );
    % sum(squeeze(sum(FEM.CAERO7.areaGrid)))
    
%     a1 = cross( (FEM.CAERO7(i).te_in - FEM.CAERO7(i).le_in)   , (FEM.CAERO7(i).le_out - FEM.CAERO7(i).le_in));
%     a2 = cross( (FEM.CAERO7(i).le_out - FEM.CAERO7(i).te_out) , (FEM.CAERO7(i).te_in - FEM.CAERO7(i).te_out));
%     FEM.CAERO7(i).areas = .5*( sqrt(sum(a1.^2)) + sqrt(sum(a2.^2)) );
%     keyboard
    
    %% Boxes
    newBoxes = (FEM.CAERO7(i).NSPAN-1)*(FEM.CAERO7(i).NCHORD-1);
    boxCount = boxCount + newBoxes;
	box_nums = FEM.CAERO7(i).WID:( FEM.CAERO7(i).WID+newBoxes - 1);
    FEM.boxes = [FEM.boxes,[box_nums; i*ones(1,newBoxes); 1:newBoxes]];
    
    boundVortex = FEM.CAERO7(i).bound_out - FEM.CAERO7(i).bound_in;
    boundVortex_n =sqrt(sum((boundVortex).^2));
    FEM.boxBoundVtxLength = [FEM.boxBoundVtxLength,boundVortex_n];
    FEM.boxQuarterPanelPoint = [FEM.boxQuarterPanelPoint,FEM.CAERO7(i).collocation];
    FEM.boxBoundVtxUnitVec = [FEM.boxBoundVtxUnitVec,boundVortex./repmat(boundVortex_n,[3,1])];

    %% Camber and Thickness
    if FEM.CAERO7(i).PAFOIL7 == 0

        flatPlate = 1;
        
        ITAX = [0 1];
        ITHR = [0 0];
        ITHT = [0 0];
        ICAMR = [0 0];
        ICAMT = [0 0];
        
    else
        flatPlate = 0;
        
        pafoil7_ind = find(FEM.LIST.PAFOIL7.ID == FEM.CAERO7(i).PAFOIL7);
        if length(pafoil7_ind) > 1
            error(['More than one PAFOIL7 with ID = ',num2str(FEM.CAERO7(i).PAFOIL7)])
        end
        ITAX = ( FEM.AEFACT(  find(FEM.LIST.AEFACT.SID == FEM.PAFOIL7(pafoil7_ind).ITAX)  ).VALUE ) ./ 100;
        ITHR = ( FEM.AEFACT(  find(FEM.LIST.AEFACT.SID == FEM.PAFOIL7(pafoil7_ind).ITHR)  ).VALUE ) ./ 100;
        ITHT = ( FEM.AEFACT(  find(FEM.LIST.AEFACT.SID == FEM.PAFOIL7(pafoil7_ind).ITHT)  ).VALUE ) ./ 100;
        ICAMR =( FEM.AEFACT(  find(FEM.LIST.AEFACT.SID == FEM.PAFOIL7(pafoil7_ind).ICAMR) ).VALUE ) ./ 100;
        ICAMT =( FEM.AEFACT(  find(FEM.LIST.AEFACT.SID == FEM.PAFOIL7(pafoil7_ind).ICAMT) ).VALUE ) ./ 100;
        
    end
    
    method = 'pchip';
    rootPanelCamber_d = interp1(FEM.CAERO7(i).RCH*ITAX + panelPlaneCorners(1,1,1),FEM.CAERO7(i).RCH*ICAMR,rootChordPoints(1,:),method);
    rootPanelCamber = rootChordPoints + repmat(thick_normal,[1,FEM.CAERO7(i).NCHORD]) .* repmat(rootPanelCamber_d,[3,1]) ;
    
    tipPanelCamber_d = interp1(FEM.CAERO7(i).TCH*ITAX + panelPlaneCorners(1,end,1),FEM.CAERO7(i).TCH*ICAMT,tipChordPoints(1,:),method);
    tipPanelCamber = tipChordPoints + repmat(thick_normal,[1,FEM.CAERO7(i).NCHORD]) .* repmat(tipPanelCamber_d,[3,1]) ;
    
    rootForceCamber_d = interp1(FEM.CAERO7(i).RCH*ITAX + panelPlaneCorners(1,1,1),FEM.CAERO7(i).RCH*ICAMR,rootForce(1,:),method);
    rootForceCamber = rootForce + repmat(thick_normal,[1,FEM.CAERO7(i).NCHORD-1]) .* repmat(rootForceCamber_d,[3,1]) ;
    
    tipForceCamber_d = interp1(FEM.CAERO7(i).TCH*ITAX + panelPlaneCorners(1,end,1),FEM.CAERO7(i).TCH*ICAMT,tipForce(1,:),method);
    tipForceCamber = tipForce + repmat(thick_normal,[1,FEM.CAERO7(i).NCHORD-1]) .* repmat(tipForceCamber_d,[3,1]) ;
    
    cambered_bound_vortex_lattice = zeros(3,FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD-1);
    cambered_panels = zeros(3,FEM.CAERO7(i).NSPAN,FEM.CAERO7(i).NCHORD);
    
    for j = 1:FEM.CAERO7(i).NCHORD-1
        cambered_bound_vortex_lattice(:,:,j)=PlaneIntersectLine(leSpanPoints,nvecPlane,rootForceCamber(:,j),tipForceCamber(:,j)-rootForceCamber(:,j));
    end
    for j = 1:FEM.CAERO7(i).NCHORD  %             (rPlane,     nvecPlane,rLine,               tanvLine)
        cambered_panels(:,:,j)=PlaneIntersectLine(leSpanPoints,nvecPlane,rootPanelCamber(:,j),tipPanelCamber(:,j)-rootPanelCamber(:,j));
    end
    
    FEM.CAERO7(i).cambered_collocation = [];
    FEM.CAERO7(i).cambered_te_in = [];
    FEM.CAERO7(i).cambered_te_out = [];
    for j = 1:FEM.CAERO7(i).NSPAN-1
        FEM.CAERO7(i).cambered_collocation = [FEM.CAERO7(i).cambered_collocation, (squeeze(cambered_bound_vortex_lattice(:,j  ,1:FEM.CAERO7(i).NCHORD-1)) + squeeze(cambered_bound_vortex_lattice(:,j+1,1:FEM.CAERO7(i).NCHORD-1)) )./2];
        FEM.CAERO7(i).cambered_te_in  = [FEM.CAERO7(i).cambered_te_in,  squeeze(cambered_panels(:,j  ,2:FEM.CAERO7(i).NCHORD  )) ];
        FEM.CAERO7(i).cambered_te_out = [FEM.CAERO7(i).cambered_te_out, squeeze(cambered_panels(:,j+1,2:FEM.CAERO7(i).NCHORD  )) ];
    end
    % find normal vector
    ac = FEM.CAERO7(i).cambered_collocation - FEM.CAERO7(i).cambered_te_in;
    bc = FEM.CAERO7(i).cambered_collocation - FEM.CAERO7(i).cambered_te_out;
    n_vec = cross(ac,bc);
    mag = sqrt( sum( n_vec.^2) );
    if n_vec'*thick_normal < 0 
        mag = -mag; % ensure normal is in same direction as macro normal
    end
    FEM.CAERO7(i).n_vec = n_vec./[mag; mag; mag];
    
    
    % Save data from plotting only
    FEM.CAERO7(i).panelPlaneCorners = panelPlaneCorners;
    FEM.CAERO7(i).cambered_panels = cambered_panels;
end

end

%% complex step friendly norm
function p = norm_cs(v)
    p = sqrt(v(1).^2+v(2).^2+v(3).^2);
end
