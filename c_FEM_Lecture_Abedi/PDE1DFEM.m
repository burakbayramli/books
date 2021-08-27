classdef PDE1DFEM
    properties
        %%%%% INPUTS to the model
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        PDEtype = 0;    % 0: elliptic, 1: parabolic, 2: hyperbolic
        computeKEigen = 0;
        % parameters needed for DG (and internal penalty) methods
        eps = 0; % epsilon, takes the value -1, 0, 1
        sigma = 0; % between element alpha = sigma / hMin
        beta = 0;
        gamma = 0;
        
        %%% hyperbolic parameters
        hyperStartOption = 0;   % 0 average, 1 Riemann
        hyper1FLambdaScalingOn = 0;
        hyperNumberOfFields = 1;    % only 1 field formulation will be implemented
        % lambda \int sigmaHat . n (v* - v) ds
        % 0 lambda = 1 (no dimensional scaling)
        % 1 hAve/ws is used for this face integral
        
        % number of elements
        ne;
        nePerRegion;
        hPerRegion;
        
        % element lengths
        numRegions = 1;
        numberOfElementsMode = 1; %0: numGiven for the whole domain / 1: for each region
        numberOfElements;         % 1 entry if above is 1 | numRegions entry if above is 0s
        LsPerRegion;              % lengths per region
        % element Factors of u,t (parabolic e.g. conductivity) |
        % OR u,tt (hyperbolic e.g. rho)
        CsPerRegion;
        % element factors of -u,xx (parabolic diffusion) / elliptic &
        % hyperbolic stiffness
        kappasPerRegion;
        %%%% hyperbolic parameters
        dampingsPerRegion; % PDE is Cu'' + damping u' - kappa u,xx = Q
        
        p = 1; % element orders
        loadCase = 0;
        flags = [0 0];  % BCs on the left and right sides, 0: essential | 1: natural
        finalTime = -1;
        stabilityDelTFactor = 1;    % for dynamic (parabolic, hyperbolic) is the factor multiplying minElement stability limit
        
        printMessages = 1;
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % values computed from inputs and outputs of the class
        L = -1;
        % element lengths
        hs = [];
        % element Factors of u,t (parabolic e.g. conductivity) |
        % OR u,tt (hyperbolic e.g. rho)
        Cs;
        % element factors of -u,xx (parabolic diffusion) / elliptic &
        % hyperbolic stiffness
        kappas;
        %%%% hyperbolic parameters
        dampings; % PDE is Cu'' + damping u' - kappa u,xx = Q
        % wave speeds
        wss; % ws = sqrt(kappa/C)
        Zs;  % impedances Z = ws * kappa
        
        nnodes;
        %  dofMap(ei, 1) and dofMap(ei, 2) are start and end dofs of an
        %  element
        dofMap;
        % number of dofs
        ndof;
        % size ne + 1: nodal coordinates
        nodalXs;
        
        % stability limit
        % minimum elmeent size
        hmin;
        deltBases; % list of basic time steps for all elements
        deltBase;  % min of deltBase
        delt;
        delts;
        times;
        numTimes;
        differentDelt4FinalTime;
        % cell of solutions for different time steps (dynamic problems)
        % size numTimes, for elliptic size 1
        As;
        
        % stiffness (conductivity)
        K;
        % eigenvectors, eigenvalues, min and max of eigenvectors
        KV;
        KD;
        KminD;
        KmaxD;
        KminDReal;
        KmaxDReal;
        % mass (capacity) - only computed for PDE type ~= 0
        M;
        MInv;
        MInvK;
        
        %%% hyperbolic
        CMat;  % Ma'' + CMat a' + Ka = F
        MpDeltCInv; % only computed for constant delt parts
        MpDeltCInvM; % MpDeltCInv * M
        MpDeltCInvC; % MpDeltCInv * CMat
        MpDeltCInvK; % MpDeltCInv * K
        % vector of element level transfer matrices for Q forces
        eRs;
    end
    methods
        function objout = read(obj, fid, extraPara)
            if ((nargin < 3) || (length(extraPara) == 0))
                %%%% elliptic / parabolic
                % para1: epsilon, para2: sigma
                %%%% hyperbolic
                % para1: RiemannStar off/on, para2: lambaScaling off/on
                para1 = -100;
                para2 = -1;
                extraPara = [para1, para2];
            end
            objout = PDE1DFEM;
            buf = fscanf(fid, '%s', 1);
            objout.PDEtype = fscanf(fid, '%d', 1);
            buf = fscanf(fid, '%s', 1);
            objout.computeKEigen = fscanf(fid, '%d', 1);
            buf = fscanf(fid, '%s', 1);
            objout.eps = fscanf(fid, '%d', 1);
            if (objout.PDEtype ~= 2)
                if (extraPara(1) ~= -100)
                    objout.eps = extraPara(1);
                end
            end
            buf = fscanf(fid, '%s', 1);
            objout.sigma = fscanf(fid, '%g', 1);
            if (objout.PDEtype ~= 2)
                if (extraPara(2) > -0.5)
                    objout.sigma = extraPara(2);
                end
            end
            buf = fscanf(fid, '%s', 1);
            objout.beta = fscanf(fid, '%g', 1);
            buf = fscanf(fid, '%s', 1);
            objout.gamma = fscanf(fid, '%g', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.hyperStartOption = fscanf(fid, '%d', 1);
            if (objout.PDEtype == 2)
                if (extraPara(1) ~= -100)
                    objout.hyperStartOption = extraPara(1);
                end
            end
            buf = fscanf(fid, '%s', 1);
            objout.hyper1FLambdaScalingOn = fscanf(fid, '%d', 1);
            if (objout.PDEtype == 2)
                if (extraPara(2) > -0.5)
                    objout.hyper1FLambdaScalingOn = extraPara(2);
                end
            end
            buf = fscanf(fid, '%s', 1);
            objout.hyperNumberOfFields = fscanf(fid, '%d', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.p = fscanf(fid, '%d', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.numRegions = fscanf(fid, '%d', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.numberOfElementsMode = fscanf(fid, '%d', 1);
            
            buf = fscanf(fid, '%s', 1);
            if (objout.numberOfElementsMode == 0)
                objout.numberOfElements = fscanf(fid, '%d', 1);
            else
                objout.numberOfElements = fscanf(fid, '%d', objout.numRegions);
            end
            
            buf = fscanf(fid, '%s', 1);
            while (strcmp(buf, 'LsPerRegion') == 0)
                buf = fscanf(fid, '%s', 1);
            end
            objout.LsPerRegion = fscanf(fid, '%g', objout.numRegions);
            for i = 1:objout.numRegions
                if (objout.LsPerRegion(i) < -999)
                    factorPi = -objout.LsPerRegion(i) / 1000;
                    if (abs(round(factorPi) - factorPi) < 1e-6)
                        objout.LsPerRegion(i) = factorPi * pi;
                    end
                end
            end
            buf = fscanf(fid, '%s', 1);
            objout.CsPerRegion = fscanf(fid, '%g', objout.numRegions);
            buf = fscanf(fid, '%s', 1);
            objout.kappasPerRegion = fscanf(fid, '%g', objout.numRegions);
            buf = fscanf(fid, '%s', 1);
            objout.dampingsPerRegion = fscanf(fid, '%g', objout.numRegions);
            
            objout.L = sum(objout.LsPerRegion);
            
            buf = fscanf(fid, '%s', 1);
            objout.loadCase = fscanf(fid, '%d', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.flags = fscanf(fid, '%d', 2);
            if (objout.flags(1) == 2)
                objout.flags(2) = 2;
            end
            if (objout.flags(2) == 2)
                objout.flags(1) = 2;
            end
            buf = fscanf(fid, '%s', 1);
            objout.finalTime = fscanf(fid, '%g', 1);
            
            buf = fscanf(fid, '%s', 1);
            objout.stabilityDelTFactor = fscanf(fid, '%g', 1);
        end
        % must be called even number of elements is not changed
        function objout = setNumElements(obj, newElementSize)
            if ((nargin == 2) && (newElementSize > 0))
                obj.numberOfElements = newElementSize;
            end
            if (obj.numberOfElementsMode == 1)
                obj.nePerRegion = obj.numberOfElements * ones(1, obj.numRegions);
            else
                for i = 1:obj.numRegions
                    obj.nePerRegion(i) = ceil(obj.numberOfElements * obj.LsPerRegion(i) / obj.L);
                end
            end
            obj.ne = sum(obj.nePerRegion);
            obj.nnodes = obj.ne + 1;
            
            cntr = 1;
            for ri = 1:obj.numRegions
                obj.hPerRegion(ri) = obj.LsPerRegion(ri) / obj.nePerRegion(ri);
                for j = 1:obj.nePerRegion(ri)
                    obj.hs(cntr) = obj.hPerRegion(ri);
                    obj.Cs(cntr) = obj.CsPerRegion(ri);
                    obj.kappas(cntr) = obj.kappasPerRegion(ri);
                    obj.dampings(cntr) = obj.dampingsPerRegion(ri);
                    cntr = cntr + 1;
                end
            end
            
            obj.nodalXs(1) = 0;
            for i = 1:obj.ne
                h = obj.hs(i);
                obj.nodalXs(i + 1) = obj.nodalXs(i) + h;
            end
            
            obj.hmin = min(obj.hs);
            if (obj.PDEtype == 1) %% parabolic PDE
                obj.deltBases = 0.5 * obj.hs .* obj.hs / obj.kappas .* obj.Cs;
            elseif  (obj.PDEtype == 2) % hyperbolic PDE
                obj.wss = sqrt(obj.kappas ./ obj.Cs);
                obj.Zs = obj.wss .* obj.Cs;
                obj.deltBases = obj.hs  ./ obj.wss;
            end
            obj.deltBase = min(obj.deltBases);
            if (obj.PDEtype ~= 0) %% dynamic PDE
                obj.delt = obj.deltBase * obj.stabilityDelTFactor;
                obj.delt = obj.finalTime / ceil(obj.finalTime / obj.delt);
                obj.times = 0:obj.delt:(obj.finalTime + obj.delt * 1e-7);
                obj.numTimes = length(obj.times);
                
                % this part should check correctly (del < 1e-3) the way I have done it now...
                del = (obj.finalTime - obj.times(obj.numTimes))/obj.delt;
                obj.differentDelt4FinalTime = 0;
                if (del > 1e-3)
                    obj.numTimes = obj.numTimes + 1;
                    obj.times(obj.numTimes) = obj.finalTime;
                    obj.differentDelt4FinalTime = 1;
                    fprintf(1, 'this part should check correctly (del < 1e-3) the way I have done it now... press any key to continue or check the cause\n');
                    pause;
                end
                for i = 1:obj.numTimes - 1;
                    obj.delts(i) = obj.times(i + 1) - obj.times(i);
                end
            end
            objout = obj;
        end
        % must be called even number of elements is not changed
        function objout = ComputeDG_KM(obj)
            obj.ndof = 2 * obj.ne;
            obj.dofMap = zeros(obj.ne, 2);
            obj.K = zeros(obj.ndof, obj.ndof);
            for ei = 1:obj.ne
                obj.dofMap(ei, 1) = 2 * ei - 1;
                obj.dofMap(ei, 2) = 2 * ei;
            end
            obj.eRs = cell(1, obj.ne);
            for ei = 1:obj.ne
                h = obj.hs(ei);
                % transfer for Q
                obj.eRs{ei}(1, 1) = 0.5 * h;
                obj.eRs{ei}(1, 2) = 0.5 * h;
                obj.eRs{ei}(2, 1) = h / 6.0;
                obj.eRs{ei}(2, 2) = h / 3.0;
            end
            if (obj.PDEtype ~= 2) % not hyperbolic - computing elliptic / parabolic stiffness
                %%% A. Interior
                for ei = 1:obj.ne
                    kappa = obj.kappas(ei);
                    h = obj.hs(ei);
                    ke = zeros(2, 2);
                    ke(2, 2) = kappa / h;
                    for i = 1:2
                        I = obj.dofMap(ei, 1) + i - 1;
                        for j = 1:2
                            J = obj.dofMap(ei, 1) + j - 1;
                            obj.K(I, J) = obj.K(I, J) + ke(i, j);
                        end
                    end
                end
                % B. Interface between interior elements
                % handling periodic boundary
                elementMax = obj.ne - 1;
                if (obj.flags(1) == 2)
                    elementMax = elementMax + 1;
                end
                for ei = 1:elementMax
                    em = ei;
                    ep = ei + 1;
                    interfaceMap = obj.dofMap(em, 1): obj.dofMap(em, 1) + 3;
                    if (ep > obj.ne)
                        ep = 1;
                        interfaceMap(3) = 1;
                        interfaceMap(4) = 2;
                    end
                    km = obj.kappas(em);
                    kp = obj.kappas(ep);
                    hm = obj.hs(em);
                    hp = obj.hs(ep);
                    kohm = km / hm;
                    kohp = kp / hp;
                    hmin = min(hm, hp);
                    alpha = obj.sigma / hmin;
                    %% alpha [[T^]] [[T]]
                    if (obj.printMessages == 1)
                        fprintf(1, 'This is an example of alpha [[T^]] [[T]] term need to compute other k terms as not computed below and add them to interface k\n');
                        pause;
                    end
                    kalpha = alpha * [1 1 -1 0;1 1 -1 0; -1 -1 1 0; 0 0 0 0];
                    if (obj.printMessages == 1)
                        fprintf(1, 'You can ignore beta and gamma term contirbutions ...\n');
                        pause;
                    end
                    kInterface = kalpha  %+ k2 + k2Trans + k3 + k3Trans;
                    for i = 1:4
                        I = interfaceMap(i);
                        for j = 1:4
                            J = interfaceMap(j);
                            obj.K(I, J) = obj.K(I, J) + kInterface(i, j);
                        end
                    end
                end
                % C. Essential BC
                if (obj.flags(1) == 0)  %% left BC
                    kappa = obj.kappas(1);
                    h = obj.hs(1);
                    ke = kappa / h * [0 1; -obj.eps 0];
                    ei = 1;
                    for i = 1:2
                        I = obj.dofMap(ei, 1) + i - 1;
                        for j = 1:2
                            J = obj.dofMap(ei, 1) + j - 1;
                            obj.K(I, J) = obj.K(I, J) + ke(i, j);
                        end
                    end
                end
                if (obj.flags(2) == 0) %% Right BC
                    ei = obj.ne;
                    if (obj.printMessages == 1)
                        fprintf(1, 'Do a similar thing to (obj.flags(1) == 0)\n');
                        pause;
                    end
                end
            end
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if (obj.PDEtype == 2) % hyperbolic - computing K and CMat, M is computed below ...
                if (obj.hyperNumberOfFields ~= 1)
                    printf(1, '% hyperbolic PDE only solved for 1F formulation now\n');
                end
                obj.CMat = zeros(obj.ndof, obj.ndof);
                %%% A. Interior
                if (obj.printMessages == 1)
                    fprintf(1, 'Assemble element level ks and cs (cs damping terms are assembled as an example)\n');
                    pause;
                end
                for ei = 1:obj.ne
                    kappa = obj.kappas(ei);
                    h = obj.hs(ei);
                    % damping part
                    damping = obj.dampings(ei);
                    if (damping > 0)
                        dh = damping * h;
                        ce(1, 1) = dh;
                        ce(1, 2) = 0.5 * dh;
                        ce(2, 1) = 0.5 * dh;
                        ce(2, 2) = dh / 3.0;
                        for i = 1:2
                            I = obj.dofMap(ei, 1) + i - 1;
                            for j = 1:2
                                J = obj.dofMap(ei, 1) + j - 1;
                                obj.CMat(I, J) = obj.CMat(I, J) + ce(i, j);
                            end
                        end
                    end
                end
                % B. Interface between interior elements
                % handling periodic boundary
                elementMax = obj.ne - 1;
                if (obj.flags(1) == 2)
                    elementMax = elementMax + 1;
                end
                for ei = 1:elementMax
                    em = ei;
                    ep = ei + 1;
                    interfaceMap = obj.dofMap(em, 1): obj.dofMap(em, 1) + 3;
                    if (ep > obj.ne)
                        ep = 1;
                        interfaceMap(3) = 1;
                        interfaceMap(4) = 2;
                    end
                    km = obj.kappas(em);
                    hm = obj.hs(em);
                    
                    kohm = km / hm;
                    lambdam = 1.0;
                    % computing lambdas
                    if (obj.hyper1FLambdaScalingOn == 1)
                        hAve = (hm + hp) / 2.0;
                        wsm = obj.wss(em);
                        lambdam = hAve / wsm;
                    end
                    
                    jumpU = [1 1 -1 0]';
                    lambdaSigma_nm = [0 kohm * lambdam 0 0]';
                    sigmam = [0 kohm 0 0];
                    vm = [1 1 0 0];
                    if (obj.printMessages == 1)
                        fprintf(1, 'Compute plus (p) side terms and lambdaSigma_n = lambdaSigma_nm + lambdaSigma_np\n');
                        pause;
                    end
                    %                    lambdaSigma_n = lambdaSigma_nm + lambdaSigma_np;
                    
                    if (obj.hyperStartOption == 0)   % 0 average
                        Sigma_sigm = 0.5;
                        Sigma_sigp = 0.5;
                        Sigma_velm = 0.0;
                        Sigma_velp = 0.0;
                        
                        Vel_sigm = 0.0;
                        Vel_sigp = 0.0;
                        Vel_velm = 0.5;
                        Vel_velp = 0.5;
                    elseif (obj.hyperStartOption == 1)   % 1s Riemann
                        Zm = obj.Zs(em);
                        Zp = obj.Zs(ep);
                        if (obj.printMessages == 1)
                            fprintf(1, 'compute Sigma_xy, Vel_xy, where x = sig or vel and y = p,m like average case\n');
                            pause;
                        end
                        ooZmZp = 1.0 / (Zm + Zp);
                        
                        Sigma_sigm = Zp * ooZmZp;
                        
                        Vel_sigm = -ooZmZp;
                    end
                    
                    cInterface = -(jumpU * (Sigma_velm * vm + 0) + ...
                        lambdaSigma_n * (Vel_velm * vm + Vel_velp * vp) - ...
                        lambdaSigma_nm * vm - lambdaSigma_np * vp);
                    
                    if (obj.printMessages == 1)
                        fprintf(1, 'compute and assemble kInterface as cInterface is computed and assembled (many k terms are not included)\n');
                        pause;
                    end
                    kInterface = -(jumpU * (Sigma_sigm * sigmam + 0) + ...
                        );
                    for i = 1:4
                        I = interfaceMap(i);
                        for j = 1:4
                            J = interfaceMap(j);
                            % K term assembly not done
                            obj.CMat(I, J) = obj.CMat(I, J) + cInterface(i, j);
                        end
                    end
                end
                % C. Essential BC
                if (obj.flags(1) == 0)  %% left BC
                    ei = 1;
                    kappa = obj.kappas(ei);
                    h = obj.hs(ei);
                    lambda = 1.0;
                    if (obj.hyper1FLambdaScalingOn == 1)
                        ws = obj.wss(em);
                        lambda = h / ws;
                    end
                    
                    ce = -kappa / h * lambda * [0 0; 1 0];
                    if (obj.printMessages == 1)
                        fprintf(1, 'compute and assemble kBC left as c is computed and assembled\n');
                        pause;
                    end
                    % ... do the K term here
                    
                end
                if (obj.flags(2) == 0) %% Right BC
                    ei = obj.ne;
                    kappa = obj.kappas(ei);
                    if (obj.printMessages == 1)
                        fprintf(1, 'Do a similar thing to the left (1) boundary for the right (2) essential BC here (k (and c)) contributions\n');
                        pause;
                    end
                    h = obj.hs(ei);
                    lambda = 1.0;
                    if (obj.hyper1FLambdaScalingOn == 1)
                        ws = obj.wss(em);
                        lambda = h / ws;
                    end
                    % take care of k, c for this boundary
                end
            end
            %%% mass matrix
            if (obj.PDEtype ~= 0) % not elliptic
                obj.M = zeros(obj.ndof, obj.ndof);
                obj.MInv = zeros(obj.ndof, obj.ndof);
                %%% Interior
                for ei = 1:obj.ne
                    C = obj.Cs(ei);
                    h = obj.hs(ei);
                    Ch = C * h;
                    if (obj.printMessages == 1)
                        fprintf(1, 'Assemble M matrix similar to K (and C) for dynamic problems\n');
                        pause;
                    end
                    me(1, 1) = Ch;
                    me(1, 2) = 0.5 * Ch;
                    % (2, 1), (2, 2) components ...
                    meInv = inv(me);
                    %% assembly of M and MInv terms below are removed
                end
            end
            % DON'T REMOVE
            %% computing eigenvalues of K for the analysis of the method ...
            if (obj.computeKEigen)
                [obj.KV, KD] = eig(obj.K);
                obj.KD = diag(KD);
                obj.KminD = obj.KD(1);
                obj.KmaxD = obj.KD(obj.ndof);
                KDReal = real(obj.KD);
                KDReal = sort(KDReal);
                obj.KminDReal = KDReal(1);
                obj.KmaxDReal = KDReal(obj.ndof);
            end
            %% matrices needed for time marching of parabolic and hyperbolic PDE
            if (obj.PDEtype == 1) % parabolic
                obj.MInvK = obj.MInv * obj.K;
            elseif (obj.PDEtype == 2) % hyperbolic
                obj.MpDeltCInv = inv(obj.M + obj.delt * obj.CMat);
                obj.MpDeltCInvM = obj.MpDeltCInv * obj.M;
                obj.MpDeltCInvC = obj.MpDeltCInv * obj.CMat;
                obj.MpDeltCInvK = obj.MpDeltCInv * obj.K;
            end
            objout = obj;
        end
        % F is force vector,
        % A is the solution
        % slnDGXs, slnDGYs slns for individual elements (discontinuity
        % preserved)
        %
        function F = ComputeDG_F(obj, time)
            if (nargin < 2)
                time = 0;
            end
            F = zeros(obj.ndof, 1);
            % A. source term
            Q = PDE1DSourceData(obj.nodalXs, obj.loadCase, obj.PDEtype, time);
            if (norm(Q) > 0)
                for ei = 1:obj.ne
                    Qe(1) = Q(ei);
                    Qe(2) = Q(ei + 1);
                    fr = obj.eRs{ei} * Qe';
                    for i = 1:2
                        I = obj.dofMap(ei, 1) + i - 1;
                        F(I) = F(I) + fr(i);
                    end
                end
            end
            % B. BC
            data = PDE1DBCData(obj.loadCase, time);
            if (obj.PDEtype ~= 2) % not hyperbolic
                for bc = 1:2
                    ei = 1 + (bc - 1) * (obj.ne - 1);
                    % B1. Essential BC
                    if (obj.flags(bc) == 0)
                        kappa = obj.kappas(ei);
                        h = obj.hs(ei);
                        f = data(bc) * obj.eps * kappa / h * [0; 1];
                        if (bc == 1)
                            f = -f;
                        end
                        % B2. Natural BC
                    elseif (obj.flags(bc) == 1)
                        if (obj.printMessages == 1)
                            fprintf(1, 'compute natural BC force on boundary bc (1 or 2)\n');
                            pause;
                        end
                        % compute element f and assemble it to global F
                    end
                end
            else % hyperbolic
                for bc = 1:2
                    ei = 1 + (bc - 1) * (obj.ne - 1);
                    % B1. Essential BC
                    if (obj.printMessages == 1)
                        fprintf(1, 'compute F contributions of essential and natural BCs for hyperbolic PDEs\n');
                        pause;
                    end
                    if (obj.flags(bc) == 0)
                        kappa = obj.kappas(ei);
                        h = obj.hs(ei);
                        lambda = 1.0;
                        if (obj.hyper1FLambdaScalingOn == 1)
                            ws = obj.wss(ei);
                            lambda = h / ws;
                        end
                        f = data(bc) * lambda * kappa / h * [0; 1];
                        if (bc == 1)
                            f = -f;
                        end
                        % B2. Natural BC
                    elseif (obj.flags(bc) == 1)
                        %% compute f for bc == 1 or bc == 2, and add it to global F ...
                    end
                end
            end
        end
        function [T0, T0Dot] = ComputeDG_IC(obj)
            time = 0;
            %
            T0 = zeros(obj.ndof, 1);
            T0Dot = zeros(obj.ndof, 1);
            [y0, y0Dot] = PDE1DICData(obj.nodalXs, obj.loadCase, obj.PDEtype);
            if (obj.PDEtype == 1) % parabolic PDE
                for ei = 1:obj.ne
                    dof1 = obj.dofMap(ei, 1);
                    dof2 = obj.dofMap(ei, 2);
                    %                h = obj.hs(ei);
                    y01 = y0(ei);
                    y02 = y0(ei + 1);
                    T0(dof1) = y01;
                    T0(dof2) = (y02 - y01);
                end
            elseif (obj.PDEtype == 2) % hyperbolic PDE
                for ei = 1:obj.ne
                    dof1 = obj.dofMap(ei, 1);
                    dof2 = obj.dofMap(ei, 2);
                    %                    h = obj.hs(ei);
                    y01 = y0(ei);
                    y02 = y0(ei + 1);
                    yDot01 = y0Dot(ei);
                    yDot02 = y0Dot(ei + 1);
                    
                    T0(dof1) = y01;
                    T0(dof2) = (y02 - y01);
                    
                    T0Dot(dof1) = yDot01;
                    T0Dot(dof2) = (yDot02 - yDot01);
                end
            end
        end
        function [slnDGXs, slnDGYs, slnXs, slnYs] = ComputeSln(obj, A)
            nVals = zeros(obj.nnodes, 1);
            slnXs = obj.nodalXs;
            slnYs = zeros(1, obj.nnodes);
            for ei = 1:obj.ne
                I = obj.dofMap(ei, 1);
                slnDGXs(I) = obj.nodalXs(ei);
                slnDGXs(I + 1) = obj.nodalXs(ei + 1);
                a = A(I);
                b = A(I + 1);
                slnDGYs(I) = a;
                slnDGYs(I + 1) = a + b;
                
                slnYs(ei) = slnYs(ei) + slnDGYs(I);
                slnYs(ei + 1) = slnYs(ei + 1) + slnDGYs(I + 1);
            end
            for ni = 1:obj.nnodes - 1
                slnYs(ni) = 0.5 * slnYs(ni);
            end
        end
        function [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs, slnDGKappaYxs] = ComputeDG_Sln(obj)
            if (obj.PDEtype == 0)
                [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Elliptic(obj);
            elseif (obj.PDEtype == 1)
                [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Parabolic(obj);
            elseif (obj.PDEtype == 2)
                [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Hyperbolic(obj);
            end
            ln = length(objout.As);
            slnDGKappaYxs = zeros(1, objout.ndof);
            for i = 1:objout.ne
                h = objout.hs(i);
                kappa = objout.kappas(i);
                epsLoc = objout.As{ln}(2 * i);
                slnDGKappaYx = epsLoc / h * kappa;
                slnDGKappaYxs(2 * i - 1) = slnDGKappaYx;
                slnDGKappaYxs(2 * i) = slnDGKappaYx;
            end
        end
        function [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Elliptic(obj)
            obj = ComputeDG_KM(obj);
            F = ComputeDG_F(obj);
            A = obj.K \ F;
            obj.As{1} = A;
            [slnDGXs, slnDGYs, slnXs, slnYs] = ComputeSln(obj, A);
            objout = obj;
        end
        function [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Parabolic(obj)
            obj = ComputeDG_KM(obj);
            [obj.As{1}, T0Dot] = ComputeDG_IC(obj);
            if (obj.printMessages == 1)
                fprintf(1, 'Do time stepping for a parabolic PDE\n');
                pause;
            end
            %%% time stepping form time step 2 to last one is removed ...
            A = obj.As{obj.numTimes};
            [slnDGXs, slnDGYs, slnXs, slnYs] = ComputeSln(obj, A);
            objout = obj;
        end
        function [objout, F, A, slnDGXs, slnDGYs, slnXs, slnYs] = ComputeDG_Sln_Hyperbolic(obj)
            obj = ComputeDG_KM(obj);
            [obj.As{1}, T0Dot] = ComputeDG_IC(obj);
            F = ComputeDG_F(obj, 0);
            % Ma'' = F - Ca' - Ka at t = 0
            Maddot0 = F - obj.CMat * T0Dot - obj.K * obj.As{1};
            addot0 = obj.MpDeltCInv * Maddot0;
            delt = obj.delts(1);
            obj.As{2} = obj.As{1} + delt * T0Dot + 0.0 * delt * delt * addot0;
            
            if (obj.printMessages == 1)
                fprintf(1, 'Do time stepping for a hyperbolic PDE\n');
                pause;
            end
            %%% time stepping form time step 3 to last one is removed ...
            A = obj.As{obj.numTimes};
            [slnDGXs, slnDGYs, slnXs, slnYs] = ComputeSln(obj, A);
            objout = obj;
        end
        function [ys, ysDot, hasExact, slnDGKappaYxsExact, hasExactYxsKappa] = PDEExactSolution(obj, xs, time)
            if (obj.loadCase ~= 4)
                [ys, ysDot, hasExact, slnDGKappaYxsExact, hasExactYxsKappa] = PDEgetExactSolution(xs, obj.loadCase, time, obj.PDEtype);
            else
                hasExact = 1;
                hasExactYxsKappa = 1;
                for i = 1:length(xs)
                    x = xs(i);
                    [ys(i), ysDot(i), ux, CuDot, slnDGKappaYxsExact(i)] = getStepWaveExactSolution(obj, x, time);
                end
            end
        end
        %%% this function is used for testing hyperbolic code
        % for 1D elasticity uDot = v, ux = strain, CuDot = p (linear momentum
        % density), kappaux = stress
        function [u, uDot, ux, CuDot, kappaux] = getStepWaveExactSolution(obj, x, t)
            xInterface = obj.LsPerRegion(1);
            cl = obj.wss(1);
            cr = obj.wss(obj.ne);
            Zl = obj.Zs(1);
            Zr = obj.Zs(obj.ne);
            rhol = obj.Cs(1);
            rhor = obj.Cs(obj.ne);
            El = obj.kappas(1);
            Er = obj.kappas(obj.ne);
            xRight = obj.L;
            
            tInterface = xInterface / cl;
            % left side
            if (x < xInterface)
                u = 0.0;
                line0 = t - x / cl;
                if (line0 <= 0.0)
                    kappaux = 0.0;
                    uDot = 0.0;
                    ux = 0.0;
                    CuDot = 0.0;
                    return;
                end
                line1 = t - tInterface + (x - xInterface) / cl;
                vbefore = -1.0 / Zl;
                if (line1 <= 0.0)
                    uDot = vbefore;
                    u = line0 * uDot;
                    kappaux = 1.0;
                    ux = kappaux / El;
                    CuDot = uDot * rhol;
                    return;
                end
                line2 = t - 2.0 * tInterface - x / cl;
                if (line2 <= 0.0)
                    uDot = -2.0 / (Zl + Zr);
                    %                    u = (line1 - line0) * vbefore + uDot * (line2 - line1);
                    u = (line0 - line1) * vbefore + uDot * line1;
                    kappaux = 2.0 * Zr / (Zl + Zr);
                    ux = kappaux / El;
                    CuDot = uDot * rhol;
                    return;
                end
                fprintf(1, 'invalid values\n');
                pause;
            end
            % now x is on the right side
            line0 = t - tInterface - (x - xInterface) / cr;
            u = 0.0;
            if (line0 <= 0.0)
                kappaux = 0.0;
                uDot = 0.0;
                ux = kappaux / Er;
                CuDot = uDot * rhor;
                return;
            end
            tInterfaceR = (xRight - xInterface) / cr;
            line1 = t - (tInterface + tInterfaceR) + (x - xRight);
            if (line1 <= 0.0)
                kappaux = 2.0 * Zr / (Zl + Zr);
                uDot = -2.0 / (Zl + Zr);
                ux = kappaux / Er;
                CuDot = uDot * rhor;
                u = line0 * uDot;
                return;
            end
            fprintf(1, 'invalid values\n');
            pause;
        end
        function Test2MaterialSolution(obj)
            for t = 0.1:0.1:0.8
                for i = 1:101
                    x(i) = (i - 1)/100;
                    [u(i), uDot(i), ux(i), CuDot(i), kappaux(i)] = obj.getStepWaveExactSolution(x(i), t);
                end
                num = 1000 * t;
                str = num2str(num);
                figure(21);
                plot(x, u);
                print('-dpng', ['u', str, '.png']);
                figure(22);
                plot(x, uDot);
                print('-dpng', ['uDot', str, '.png']);
                figure(23);
                plot(x, ux);
                print('-dpng', ['ux', str, '.png']);
                figure(24);
                plot(x, CuDot);
                print('-dpng', ['CuDot', str, '.png']);
                figure(25);
                plot(x, kappaux);
                print('-dpng', ['kappaux', str, '.png']);
            end
        end
    end
end