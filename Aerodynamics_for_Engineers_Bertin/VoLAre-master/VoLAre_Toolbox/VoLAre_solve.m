% Function to solve for vortex strengths, forces, moments, and coefficients
%
% Anthony Ricciardi
%
% Inputs
% w:      Matrix of influnce coefficients.  Flow through each panel induced proportionally by each vortex [number of panels x number of panels]
% Vind14: Velocity at the 1/4 chord of each panel induced proportionally by each vortex [number of panels x number of panels x 3]
% b: right hand side vector
% V [3,1] velocity vector xByRzU coordinate system
%
% Outputs
% C   = Coefficient outputs
% Dim = Dimensionalized outputs
%
function [C,Dim,FEM] = VoLAre_solve(FEM,w,Vind14,alpha,delta)
nb = size(FEM.boxes,2);

%% Orientation
alpha_d = FEM.TRIMFLT.ALPHA + alpha;
beta_d = 0;

%% Unit Vector in Velocity Direction
R_aircraft_air = ...
    [cosd(beta_d) -sind(beta_d) 0; sind(beta_d) cosd(beta_d) 0; 0 0 1]' ...
    *[cosd(alpha_d) 0 sind(alpha_d); 0 1 0; -sind(alpha_d) 0 cosd(alpha_d)]';

V_n = R_aircraft_air*[1;0;0];

%% Right Hand Side
% save original normal vector orientations
if isfield(FEM.CAERO7,'n_vec_0') == 0
    for i = 1:size(FEM.CAERO7,2)
        FEM.CAERO7(i).n_vec_0 = FEM.CAERO7(i).n_vec;
    end
end

%% Rotate control surface normal vectors
if nargin > 4
    
    % find cs coordinate system
    cid = [];
    for i = 1:size(FEM.AESURFZ,2)
        if strcmp(FEM.TRIMFLT.LABEl1,FEM.AESURFZ(i).LABEL) == 1
            cid = 1;
        end
    end
    if isempty(cid)
        error('Can''t find AESURFZ entry with matching LABEL to trim variable')
    end
    CORD2R_ID = [];
    for i = 1:size(FEM.CORD2R,2)
        if FEM.CORD2R(i).CID == abs(FEM.AESURFZ(cid).CID)
            CORD2R_ID = i;
        end
    end
    if isempty(CORD2R_ID)
        error('Can''t find the CORD2R entry referenced in the AESURFZ entry')
    end
    if FEM.AESURFZ(cid).CID > 0
        error('Modify the code to use y as rotation axis, or use z, which is set up.')
    end
    
    delta_d = FEM.TRIMFLT.VAL1 + delta;
    
    % Make Control Surfaace Rotation Matrix
    Ap = [FEM.CORD2R(CORD2R_ID).A1,FEM.CORD2R(CORD2R_ID).A2,FEM.CORD2R(CORD2R_ID).A3];
    Bp = [FEM.CORD2R(CORD2R_ID).B1,FEM.CORD2R(CORD2R_ID).B2,FEM.CORD2R(CORD2R_ID).B3];
    Cp = [FEM.CORD2R(CORD2R_ID).C1,FEM.CORD2R(CORD2R_ID).C2,FEM.CORD2R(CORD2R_ID).C3];
    z = (Bp-Ap)./norm(Bp-Ap);
    nu = (Cp-Ap);
    y = cross(z,nu); y = y./norm(y);
    x = cross(y,z); x = x./norm(x);
    CC = [x;y;z];
    CC_rot = [cosd(delta_d) -sind(delta_d) 0; sind(delta_d) cosd(delta_d) 0; 0 0 1];
    CC_cs = CC'*CC_rot*CC;
    
    % get the panel numbers to rotate
    PANLST_ID = [];
    if isfield(FEM,'PANLST1'); error('PANLST1 entry not supported'); end
    if isfield(FEM,'PANLST2')
        PANLST_ID = [FEM.PANLST2.SETID];
        PANLST_TYPE = 2*ones(1,size(PANLST_ID,2));
        PANLST_IND = 1:size(PANLST_ID,2);
    end
    if isfield(FEM,'PANLST3')
        NEW_PANLST_ID = [FEM.PANLST3.SETID];
        NEW_PANLST_TYPE = 3*ones(1,size(NEW_PANLST_ID,2));
        NEW_PANLST_IND = 1:size(NEW_PANLST_ID,2);
        PANLST_ID = [PANLST_ID,NEW_PANLST_ID];
        PANLST_TYPE = [PANLST_TYPE,NEW_PANLST_TYPE];
        PANLST_IND = [PANLST_IND,NEW_PANLST_IND];
        clear NEW_PANLST_ID NEW_PANLST_TYPE NEW_PANLST_IND
    end
    if size(PANLST_ID,2)~=size(unique(PANLST_ID),2); error('All PANLST SETID fields must be unique'); end
    
    pind = find(PANLST_ID == FEM.AESURFZ(cid).SETK);
    if isempty(pind)
        error('Can''t find the PANLST entry referenced in the AESURFZ entry')
    end
    switch PANLST_TYPE(pind)
        case 2
            panlist = FEM.PANLST2(PANLST_IND(pind)).VALUE;
        case 3
            pcaeroList = FEM.PANLST3(PANLST_IND(pind)).VALUE;
            caeroList = {FEM.CAERO7.LABEL};
            panlist = [];
            for i = 1:size(pcaeroList,2)
                cind = find(strcmp(pcaeroList{i},caeroList));
                if length(cind)~=1
                    error('Issue with PANLST3 SETID %d.  LABEL not found or not unique.',FEM.PANLST3(PANLST_IND(pind)).SETID)
                end
                pmin = FEM.CAERO7(cind).WID;
                pmax = FEM.CAERO7(cind).WID+((FEM.CAERO7(cind).NSPAN-1)*(FEM.CAERO7(cind).NCHORD-1));
                panlist = [panlist,pmin:pmax];
                clear pmin pmax
            end
        otherwise
            error('Issue with PANLST logic');
    end
    csPanelIndexNumbers = find(ismember(FEM.boxes(1,:),panlist));
    
else
    csPanelIndexNumbers = [];
end


%% MAKE THE RIGHT HAND SIDE VECTOR
b = zeros(nb,1);
for b1 = 1:nb
    b1i = FEM.boxes(2,b1); % macro element number
    b1j = FEM.boxes(3,b1); % box in macro element
    
    if any(b1 == csPanelIndexNumbers)
        FEM.CAERO7(b1i).n_vec(:,b1j) = CC_cs * FEM.CAERO7(b1i).n_vec_0(:,b1j);
    end
    
    % Right Hand Side Vector
    b(b1) = -FEM.CAERO7(b1i).n_vec(:,b1j)'*V_n;
end

%% Solve for vortex strengths
g = w\b;

%% Solve for box forces using Kutta–Joukowski
BoundVtx = FEM.boxBoundVtxUnitVec .* repmat(g',[3,1]);
V14 = repmat(V_n',[nb,1]) + [Vind14(:,:,1)*g, Vind14(:,:,2)*g, Vind14(:,:,3)*g];
F_QINF = 2*cross(V14,BoundVtx',2).*repmat(FEM.boxBoundVtxLength',[1,3]);
bodyF_QINF = sum(F_QINF);

bodyF_QINF_aero_frame = R_aircraft_air'*(bodyF_QINF');
L_QINF = bodyF_QINF_aero_frame(3);
D_QINF = bodyF_QINF_aero_frame(1);
% % L_QINF = -bodyF_QINF(1)*sind(AoA_deg) + bodyF_QINF(3)*cosd(AoA_deg);
% % D_QINF =  bodyF_QINF(3)*sind(AoA_deg) + bodyF_QINF(1)*cosd(AoA_deg);

% moments
R = FEM.boxQuarterPanelPoint - repmat(...
    [FEM.AEROZ.REFX + FEM.TRIM.RHOX; ...
    FEM.AEROZ.REFY + FEM.TRIM.RHOY;...
    FEM.AEROZ.REFZ + FEM.TRIM.RHOZ],[1,nb]);

M_QINF =  sum(cross(R',F_QINF));

%% Total Coefficients
switch FEM.AEROZ.XZSYM
    case 'YES'
        symmetricFactor = 2;
        symmetricFactor_M = [0,2,0];
    case 'NO'
        symmetricFactor = 1;
        symmetricFactor_M = [1,1,1];
    otherwise
        error('FEM.AEROZ.XZSYM option not supported.')
end

% coefficient output
C.L = symmetricFactor   * L_QINF /(FEM.AEROZ.REFS);
C.D = symmetricFactor   * D_QINF /(FEM.AEROZ.REFS);
C.M = symmetricFactor_M.* M_QINF./(FEM.AEROZ.REFS*FEM.AEROZ.REFC);
C.N = symmetricFactor   * bodyF_QINF /(FEM.AEROZ.REFS);

if nargout > 1
    % dimesionalized output
    Dim.F     = FEM.TRIM.QINF*F_QINF;
    Dim.bodyF = FEM.TRIM.QINF*bodyF_QINF;
end

%% Pressure Array
if nargout > 2
    for i = 1:size(FEM.CAERO7,2)
        FEM.CAERO7(i).forceGrid = zeros(1,FEM.CAERO7(i).NSPAN-1,FEM.CAERO7(i).NCHORD-1);
        indx = find(FEM.boxes(2,:)==i);
        iter = 0;
        for j = 1:FEM.CAERO7(i).NSPAN-1
            for k = 1:FEM.CAERO7(i).NCHORD-1
                iter = iter + 1;
                FEM.CAERO7(i).forceGrid(1,j,k) = Dim.F( indx(iter) , : )*FEM.CAERO7(i).n_vec(:,iter);
            end
        end
    end
end

%% Optional Spanwise Data
if nargout > 2
    for i = 1:size(FEM.CAERO7,2)
        FEM.CAERO7(i).SpanwiseN    = zeros(FEM.CAERO7(i).NSPAN-1,3);
        FEM.CAERO7(i).SpanwiseLD   = zeros(FEM.CAERO7(i).NSPAN-1,3);
        FEM.CAERO7(i).Spanwise_Cl  = zeros(FEM.CAERO7(i).NSPAN-1,1);
        FEM.CAERO7(i).Spanwise_Cd  = zeros(FEM.CAERO7(i).NSPAN-1,1);
        FEM.CAERO7(i).SpanwiseLocation = zeros(FEM.CAERO7(i).NSPAN-1,3);
        
        macro_f_ind = find(FEM.boxes(3,FEM.boxes(2,:)==i)); % macro panel force indicies
        for s = 1:FEM.CAERO7(i).NSPAN-1
            FEM.CAERO7(i).SpanwiseN(s,1:3) = sum(Dim.F(macro_f_ind( (1:FEM.CAERO7(i).NCHORD-1)+(FEM.CAERO7(i).NCHORD-1)*(s-1) ) ,1:3));
            FEM.CAERO7(i).SpanwiseLD(s,1:3) = ( R_aircraft_air'*(FEM.CAERO7(i).SpanwiseN(s,1:3))' )';
            
            sInd1 = macro_f_ind(1+(FEM.CAERO7(i).NCHORD-1)*(s-1));
            sectionChord = ((FEM.CAERO7(i).trail_in_te(1,sInd1) - FEM.CAERO7(i).le_in(1,sInd1)) + (FEM.CAERO7(i).trail_out_te(1,sInd1) - FEM.CAERO7(i).le_out(1,sInd1)) )/2;
            sectionSpan = abs( FEM.CAERO7(i).le_out(2,sInd1) - FEM.CAERO7(i).le_in(2,sInd1) );
            sectionLift = FEM.CAERO7(i).SpanwiseLD(s,3);
            sectionDrag = FEM.CAERO7(i).SpanwiseLD(s,1);
            
            FEM.CAERO7(i).Spanwise_Cl(s)  = sectionLift/(FEM.TRIM.QINF*sectionChord*sectionSpan);
            FEM.CAERO7(i).Spanwise_Cd(s)  = sectionDrag/(FEM.TRIM.QINF*sectionChord*sectionSpan);
            FEM.CAERO7(i).SpanwiseLocation(s,:) = ( FEM.CAERO7(i).trail_in_te(:,sInd1) + FEM.CAERO7(i).le_in(:,sInd1) + FEM.CAERO7(i).trail_out_te(:,sInd1) + FEM.CAERO7(i).le_out(:,sInd1) )'/4;
            
        end
    end
end
end