function res = Hybrid2_AdvecRes1d_Algorithm4(qi,flux,dflux,S,dx,limiter)
%
% MUSCL-THINC-BVD implementation for scalar advection Equations 
%   -- Algorithm 4 
%
%% 1. MUSCL reconstruction 

% Initial Arrays      
% qi = q;  % : q_{ j }^{n},
qim1 = circshift(qi,+1); % : q_{j-1}^{n},
qip1 = circshift(qi,-1); % : q_{j+1}^{n}.

% Auxiliary Arrays
dqR = qip1-qi; dqL = qi-qim1; dqC = (qip1-qim1)/2; dq=zeros(size(qi));

% Compute and limit slopes
for j = 1:size(qi,2) % for all internal faces
    switch limiter
        case 'MC' % MC limiter
            % Find dq_j = minmod{fwd diff, bwd diff, cntrl diff}
            dq(j) = minmod([2*dqR(j),2*dqL(j),dqC(j)]);
        case 'MM' % Minmod limiter
            % Find dq_j = minmod{fwd diff, bwd diff}
            dq(j) = minmod([dqR(j),dqL(j)]);
        case 'VA' % Van Albada limiter
            dq(j) = vanalbada(dqR(j),dqL(j),dx);
        otherwise
            error('limiter not listed!')
    end
end

% Left and Right extrapolated q-values at the boundary j+1/2
qiph_M = qi+dq/2;	% q_{j+1/2}^{-} from cell j
qimh_M = qi-dq/2;	% q_{j+1/2}^{+} from cell j

% Compute total boundary variations TBV for each cell
TBV_M = abs(circshift(qiph_M,+1)-qimh_M)+abs(qiph_M-circshift(qimh_M,-1));

%% 2. THINC reconstruction
% Constants parameters
Beta=1.6; epsilon = 1E-20;

% Coeficients
qmin = min(cat(3,qim1,qip1),[],3);
qmax = max(cat(3,qim1,qip1),[],3)-qmin;
theta= sign(qip1-qim1);
C = (qi-qmin+epsilon)./(qmax+epsilon);
B = exp(Beta*theta.*(2*C-1));
A = (B/cosh(Beta)-1)/tanh(Beta);

% q_{i+1/2}^{-} and q_{i-1/2}^{+} reconstructions for Beta_s
qiph_T = qmin + 0.5*qmax.*(1+theta.*(tanh(Beta)+A)./(1+A*tanh(Beta)));
qimh_T = qmin + 0.5*qmax.*(1+theta.*A);

% Compute total boundary variations TBV for each cell
TBV_T = abs(circshift(qiph_T,+1)-qimh_T)+abs(qiph_T-circshift(qimh_T,-1));

%% 3. BVD Algorithm
condition = TBV_T < TBV_M;
qiph_M(condition)=qiph_T(condition); qL=circshift(qiph_M,0);
qimh_M(condition)=qimh_T(condition); qR=circshift(qimh_M,-1);

% Debug
% qL=circshift(qiph_M,0);
% qR=circshift(qimh_M,-1);

%% Compute Lax-Friedrichs numerical flux and update solution
LF = 0.5*(flux(qL)+flux(qR)-abs(dflux((qi+qip1)/2)).*(qR-qL)); % Lax friedrichs flux
res = (LF-circshift(LF,1))/dx - S(qi); % L = - df(q)/dx + S(q).

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