function res = MUSCL_EulerRes1d_Fluctuations(q,dx,limiter)
    % Commpute Euler residual using Leveque's Wave fluctuation methods
    global gamma

    % Solution Arrays
    N = size(q,2); 
    dw=zeros(3,N);
    wL=zeros(3,N); 
    wR=zeros(3,N);
    res=zeros(3,N);

    % Compute primitive variables at solution points
    w(1,:) = q(1,:);
    w(2,:) = q(2,:)./q(1,:);
    w(3,:) = (gamma-1)*( q(3,:) - 0.5*(q(2,:).^2)./q(1,:));

    % Compute and limit slopes
    for i = 1:3
        for j = 2:N-1 % for all internal faces
            switch limiter
                case 'MC' % MC limiter
                    % Find dq_j = minmod{fwd diff, bwd diff, cntrl diff}
                    dwR = 2*(w(i,j+1) - w(i,j));
                    dwL = 2*(w(i,j) - w(i,j-1));
                    dwC = (w(i,j+1) - w(i,j-1))/(2);
                    dw(i,j) = minmod([dwR,dwL,dwC]);
                case 'MM' % Minmod limiter
                    % Find dq_j = minmod{fwd diff, bwd diff}
                    dwR = (w(i,j+1) - w(i,j));
                    dwL = (w(i,j) - w(i,j-1));
                    dw(i,j) = minmod([dwR,dwL]);
                case 'VA' % Van Albada limiter
                    dwR = (w(i,j+1) - w(i,j));
                    dwL = (w(i,j) - w(i,j-1));
                    dw(i,j) = vanalbada(dwR,dwL,dx);
            end
        end
    end

    % Left and Right extrapolated q-values at the boundary j+1/2
    for j = 2:N-1 % for the domain cells
        wR(:, j ) = w(:, j ) - dw(:, j )/2;	% q_{j-1/2}^{+} from j
        wL(:,j+1) = w(:, j ) + dw(:, j )/2;	% q_{j+1/2}^{-} from j
    end
    
	% HACK! Apply boundary conditions in reconstructed arrays
    wL(:,1) = w(:,1); 
    wR(:,1) = w(:,1);
    wL(:,2) = w(:,1);
    wR(:,N-1) = w(:,N-1);
    wL(:,N) = w(:,N-1);
    wR(:,N) = w(:,N-1);
    
    % Compute conservative variables at faces
    qR(1,:) = wR(1,:);
    qR(2,:) = wR(2,:).*wR(1,:);
    qR(3,:) = wR(3,:)./(gamma-1) + 0.5*wR(1,:).*wR(2,:).^2;

    qL(1,:) = wL(1,:);
    qL(2,:) = wL(2,:).*wL(1,:);
    qL(3,:) = wL(3,:)./(gamma-1) + 0.5*wL(1,:).*wL(2,:).^2;

    % loop over cells, compute residual with wave propagation method
    for j = 2:N-1
        res(:,j) = (HLLCfluxuation(qL(:,j),qR(:,j),gamma, +1) ...
            + HLLCfluxuation(qL(:,j+1),qR(:,j+1),gamma, -1) ...
            + HLLCfluxuation(qR(:,j),qL(:,j+1),gamma, 0))/dx;
    end
end

function mm = minmod(v)
    % Using Harten's generalized definition
    % minmod: zero if opposite sign, otherwise the one of smaller magnitude.
    %m=size(v,1); mm=zeros(size(v,2),1); s=sum(sign(v),2)/m; ids=find(abs(s)==1);
    %if(~isempty(ids)); mm(ids)=s(ids).*min(abs(v(ids,:)),[],2); end
    s = sum(sign(v))/numel(v); 
    if abs(s)==1; mm = s*min(abs(v(:))); else, mm=0; end
end

function va = vanalbada(da,db,h)
    % Van Albada Slope Limiter Function
    % vanAlbada: extend the simetric formulation of the van leer limiter
    eps2=(0.3*h)^3; 
    va=0.5*(sign(da)*sign(db)+1)*((db^2+eps2)*da+(da^2+eps2)*db)/(da^2+db^2+2*eps2);
end

function HLLC = HLLCfluxuation(qL,qR,gamma,direction)
    % Compute HLLC flux
    %
    % For more details on this simple HLLC formulation see Ref:
    % Shyue, Keh-Ming, and Feng Xiao. "An Eulerian interface sharpening
    % algorithm for compressible two-phase flow: the algebraic THINC
    % approach." Journal of Computational Physics 268 (2014): 326-354.  

    % Left state
    rL = qL(1);
    uL = qL(2)./rL;
    %EL = qL(3)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    aL = sqrt(gamma*pL/rL);

    % Right state
    rR = qR(1);
    uR = qR(2)./rR;
    %ER = qR(3)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    aR = sqrt(gamma*pR/rR);

    % Compute wavespeeds
    s1 = min(uL-aL, uR-aR);
    s3 = max(uL+aL, uR+aR);
    s2 = (pR - pL + rL*uL*(s1-uL) - rR*uR*(s3-uR))/(rL*(s1-uL) - rR*(s3-uR));

    % Compute intermediate states
    uL_vec = [uL; uL; uL];
    nL = [0; 1; uL];
    uR_vec = [uR; uR; uR];
    nR = [0; 1; uR];

    n_s = [0; 1; s2];
    p_s = rL*(uL-s1)*(uL-s2) + pL;

    qL_s = ((uL_vec-s1).*qL + (pL*nL - p_s*n_s))/(s2-s1);
    qR_s = ((uR_vec-s3).*qR + (pR*nR - p_s*n_s))/(s2-s3);

    % Compute jumps
    W1 = qL_s - qL;
    W2 = qR_s - qL_s;
    W3 = qR - qR_s;

    % Wave speeds to be chosen by direction
    if direction > 0
        s1 = max(s1,0);
        s2 = max(s2,0);
        s3 = max(s3,0);
    elseif direction < 0
        s1 = min(s1,0);
        s2 = min(s2,0);
        s3 = min(s3,0);
    end

    % Sum of propagating discontinuities
    HLLC = s1*W1 + s2*W2 + s3*W3;
end
