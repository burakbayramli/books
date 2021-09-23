function res = WENO5_EulerRes1d_Fluctuations(q,dx,N)
    % Commpute Euler residual using Leveque's Wave fluctuation methods
    
    % parameters
    global gamma
    R=3; I=R:(N-R); % R: stencil size

    % Solution Arrays 
    wn=zeros(3,N); 
    wp=zeros(3,N);
    res=zeros(3,N);

    % Compute primitive variables at solution points
    w(1,:) = q(1,:);
    w(2,:) = q(2,:)./q(1,:);
    w(3,:) = (gamma-1)*( q(3,:) - 0.5*(q(2,:).^2)./q(1,:));

    % Right State Extrapolation $u_{i+1/2}^{-}$
    vmm = w(:,I-2);
    vm  = w(:,I-1);
    v   = w(:, I );
    vp  = w(:,I+1);
    vpp = w(:,I+2);

    % Smooth Indicators (Beta factors)
    B0n = 13/12*(vmm-2*vm+v  ).^2 + 1/4*(vmm-4*vm+3*v).^2; 
    B1n = 13/12*(vm -2*v +vp ).^2 + 1/4*(vm-vp).^2;
    B2n = 13/12*(v  -2*vp+vpp).^2 + 1/4*(3*v-4*vp+vpp).^2;

    % Constants
    d0n = 1/10; d1n = 6/10; d2n = 3/10; epsilon = 1e-6;

    % Alpha weights 
    alpha0n = d0n./(epsilon + B0n).^2;
    alpha1n = d1n./(epsilon + B1n).^2;
    alpha2n = d2n./(epsilon + B2n).^2;
    alphasumn = alpha0n + alpha1n + alpha2n;

    % ENO stencils weigths
    w0n = alpha0n./alphasumn;
    w1n = alpha1n./alphasumn;
    w2n = alpha2n./alphasumn;

    % Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
    wn(:,I) = w0n.*(2*vmm - 7*vm + 11*v)/6 ...
        + w1n.*( -vm  + 5*v  + 2*vp)/6 ...
        + w2n.*(2*v   + 5*vp - vpp )/6;

    % Left State Extrapolation $u_{i+1/2}^{+}$ 
    umm = w(:,I-2);
    um  = w(:,I-1);
    u   = w(:, I );
    up  = w(:,I+1);
    upp = w(:,I+2);

    % Smooth Indicators (Beta factors)
    B0p = 13/12*(umm-2*um+u  ).^2 + 1/4*(umm-4*um+3*u).^2; 
    B1p = 13/12*(um -2*u +up ).^2 + 1/4*(um-up).^2;
    B2p = 13/12*(u  -2*up+upp).^2 + 1/4*(3*u -4*up+upp).^2;

    % Constants
    d0p = 3/10; d1p = 6/10; d2p = 1/10; epsilon = 1e-6;

    % Alpha weights 
    alpha0p = d0p./(epsilon + B0p).^2;
    alpha1p = d1p./(epsilon + B1p).^2;
    alpha2p = d2p./(epsilon + B2p).^2;
    alphasump = alpha0p + alpha1p + alpha2p;

    % ENO stencils weigths
    w0p = alpha0p./alphasump;
    w1p = alpha1p./alphasump;
    w2p = alpha2p./alphasump;

    % Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
    wp(:,I+1) = w0p.*( -umm + 5*um + 2*u  )/6 ...
        + w1p.*( 2*um + 5*u  - up   )/6 ...
        + w2p.*(11*u  - 7*up + 2*upp)/6;
    
    % HACK! Apply boundary conditions in reconstructed arrays
    wn(:,1) = w(:,3);
    wn(:,2) = w(:,3);
    wn(:,3) = w(:,3);
    wn(:,N) = w(:,N-2);
    wn(:,N-1)=w(:,N-2);
    wn(:,N-2)=w(:,N-2);
    
    wp(:,1) = w(:,3);
    wp(:,2) = w(:,3);
    wp(:,3) = w(:,3);
    wp(:,N) = w(:,N-2);
    wp(:,N-1)=w(:,N-2);
    wp(:,N-2)=w(:,N-2);
    
    % Compute conservative variables at faces
    qR(1,:) = wn(1,:);
    qR(2,:) = wn(2,:).*wn(1,:);
    qR(3,:) = wn(3,:)./(gamma-1) + 0.5*wn(1,:).*wn(2,:).^2;

    qL(1,:) = wp(1,:);
    qL(2,:) = wp(2,:).*wp(1,:);
    qL(3,:) = wp(3,:)./(gamma-1) + 0.5*wp(1,:).*wp(2,:).^2;

    % loop over cells, compute residual with wave propagation method
    for j = 3:N-2
        res(:,j) = (HLLCfluxuation(qL(:,j),qR(:,j),gamma, +1) ...
            + HLLCfluxuation(qL(:,j+1),qR(:,j+1),gamma, -1) ...
            + HLLCfluxuation(qR(:,j),qL(:,j+1),gamma, 0))/dx;
    end
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
