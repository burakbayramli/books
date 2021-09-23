function [res] = THINC_EulerRes1d_Fluctuations(qi,dx,N)
%   MUSCL Monotonic Upstreat Centered Scheme for Conservation Laws
%   Van Leer's MUSCL reconstruction scheme using piece wise linear
%   reconstruction
%  
%   where: limiter='MC'; fluxMethod='AUSM';
%
%   Flux at j+1/2
% 
%     j+1/2         Cell's grid:
%   | wL|   |
%   |  /|wR |           1   2   3   4        N-2 N-1  N
%   | / |\  |   {x=0} |-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-| {x=L}
%   |/  | \ |         1   2   3   4   5        N-1  N  N+1
%   |   |  \|
%   |   |   |       NC: Here cells 1 and N are ghost cells%
%     j  j+1
%
% Written by Manuel Diaz, NTU, 04.29.2015.
global gamma

    % Allocate arrays
    wim1=zeros(size(qi));   wip1=zeros(size(qi));
    TBV_s=zeros(size(qi));  TBV_l=zeros(size(qi));
    wR=zeros(size(qi));     wL=zeros(size(qi));
    qR=zeros(size(qi));     qL=zeros(size(qi));
    res=zeros(size(qi));

    % Constants parameters
    Beta_s=1.1; Beta_l=1.6; epsilon=1E-20; J=2:N-1;
    
    % compute primitive variables at solution points
    wi(1,:) = qi(1,:);
    wi(2,:) = qi(2,:)./qi(1,:);
    wi(3,:) = (gamma-1)*( qi(3,:) - 0.5*qi(2,:).^2./qi(1,:));

    % Initial Arrays      
    % qi(:,J) = q(:, J );  % : q_{ j }^{n},
    wim1(:,J) = wi(:,J-1); % : q_{j-1}^{n},
    wip1(:,J) = wi(:,J+1); % : q_{j+1}^{n}.

    % Coeficients
    wmin = min(cat(3,wim1,wip1),[],3);
    wmax = max(cat(3,wim1,wip1),[],3)-wmin;
    theta= sign(wip1-wim1); %theta(theta==0)=1;
    C = (wi-wmin+epsilon)./(wmax+epsilon);
    B = exp(Beta_s*theta.*(2*C-1));
    A = (B/cosh(Beta_s)-1)/tanh(Beta_s);

    % q_{i+1/2}^{-} and q_{i-1/2}^{+} reconstructions for Beta_s
    wiph_s = wmin + 0.5*wmax.*(1+theta.*(tanh(Beta_s)+A)./(1+A*tanh(Beta_s)));
    wimh_s = wmin + 0.5*wmax.*(1+theta.*A);

    % Coeficients for Beta_l
    B = exp(Beta_l*theta.*(2*C-1));
    A = (B/cosh(Beta_l)-1)/tanh(Beta_l);

    % q_{i+1/2}^{-} and q_{i-1/2}^{+} reconstructions for Beta_l
    wiph_l = wmin + 0.5*wmax.*(1+theta.*(tanh(Beta_l)+A)./(1+A*tanh(Beta_l)));
    wimh_l = wmin + 0.5*wmax.*(1+theta.*A);

    % Compute total boundary variations TBV for each cell
    TBV_s(:,J) = abs(wiph_s(:,J-1)-wimh_s(:,J))+abs(wiph_s(:,J)-wimh_s(:,J+1)); 
    TBV_l(:,J) = abs(wiph_l(:,J-1)-wimh_l(:,J))+abs(wiph_l(:,J)-wimh_l(:,J+1));

    % Adaptative THINC-BVD reconstruction
    condition= ((wip1-wi).*(wi-wim1))<0;
    wiph_s(condition)=wi(condition);
    wimh_s(condition)=wi(condition);
    condition= TBV_l<TBV_s;
    wiph_s(condition)=wiph_l(condition); wL(:,J+1)=wiph_s(:,J);
    wimh_s(condition)=wimh_l(condition); wR(:,J)=wimh_s(:,J);
    
    % HACK! Apply boundary conditions in reconstructed arrays
    wL(:,1) = wi(:,1); 
    wR(:,1) = wi(:,1);
    wL(:,2) = wi(:,1);
    wR(:,N-1) = wi(:,N-1);
    wL(:,N) = wi(:,N-1);
    wR(:,N) = wi(:,N-1);
    
    % compute conservative variables at faces 
    qR(1,:) = wR(1,:);
    qR(2,:) = wR(2,:).*wR(1,:);
    qR(3,:) = wR(3,:)./(gamma-1) + wR(1,:)/2.*(wR(2,:).^2);

    qL(1,:) = wL(1,:);
    qL(2,:) = wL(2,:).*wL(1,:);
    qL(3,:) = wL(3,:)./(gamma-1) + wL(1,:)/2.*(wL(2,:).^2);

    % loop over cells, compute residual with wave propagation method
    for j = 2:N-1
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
