K = [517.3 0 318.6;	0 516.5 255.3; 0 0 1];
c2 = double(imreadbw('rgb/1305031102.175304_broken.png'));
c1 = double(imreadbw('rgb/1305031102.275326.png'));


d2 = double(imread('depth/1305031102.160407.png'))/5000;
d1 = double(imread('depth/1305031102.262886.png'))/5000;

% result:
% approximately  -0.0018    0.0065    0.0369   -0.0287   -0.0184   -0.0004

%%
K = [ 535.4  0 320.1;	0 539.2 247.6; 0 0 1];
c1 = double(imreadbw('rgb/1341847980.722988_broken.png'));
c2 = double(imreadbw('rgb/1341847982.998783.png'));


d1 = double(imread('depth/1341847980.723020.png'))/5000;
d2 = double(imread('depth/1341847982.998830.png'))/5000;

% result:
%  approximately  0.2979   -0.0106    0.0452   -0.0041   -0.0993   -0.0421

%%

% use huber weights
useHuber = true;

% exactly one of those should be true.
useGN = false; % Gauss-Newton
useLM = true; % Levenberg Marquad
useGD = false; % Gradiend descend


% initialization
xi = [0 0 0 0 0 0]';

% pyramid levels
for lvl = 5:-1:1
    % get downscaled image, depth image, and K-matrix of down-scaled image.
    [IRef, Klvl] = downscaleImage(c1,K,lvl);
    I = downscaleImage(c2,K,lvl);
    [DRef] = downscaleDepth(d1,lvl);
    lambda = 0.1;

    % just do at most 20 steps.
    errLast = 1e10;
    for i=1:30
        
        subplot(1,2,1);
        % calculate Jacobian of residual function (Matrix of dim (width*height) x 6)
        %[Jac, residual] = deriveErrNumeric(IRef,DRef,I,xi,Klvl);   % ENABLE ME FOR NUMERIC DERIVATIVES
        [Jac, residual] = deriveErrAnalytic(IRef,DRef,I,xi,Klvl);   % ENABLE ME FOR ANALYTIC DERIVATIVES
        axis equal
        
        % just take the pixels that have no NaN (e.g. because
        % out-of-bounds, or because the didnt have valid depth).
        valid = ~isnan(sum(Jac,2));
        residualTrim = residual(valid,:);
        JacTrim = Jac(valid,:);
        

        if(useHuber)
            % compute Huber Weights
            huber = ones(size(residual));
            huberDelta = 4/255;
            huber(abs(residual) > huberDelta) = huberDelta ./ abs(residual(abs(residual) > huberDelta));
            
            
            % plot Huber Weights
            subplot(1,2,2);
            imagesc(reshape(huber, size(I)));
            axis equal
            huberTrim = huber(valid);
        end

        
        
        
        if(useGN)
            % do Gauss-Newton step
            upd = - (JacTrim' * (repmat(huberTrim,1,6) .* JacTrim))^-1 * JacTrim' * (huberTrim .* residualTrim);
        end
        
        if(useGD)
            % do gradient descend
            upd = - JacTrim' * (huberTrim .* residualTrim);
            upd = 0.001 * upd / norm(upd)   % choose step size such that the step is always 0.001 long.
        end
        
        
        if(useLM)
            % do LM
            H = (JacTrim' * (repmat(huberTrim,1,6) .* JacTrim));
            upd = - (H + lambda * diag(diag(H)))^-1 * JacTrim' * (huberTrim .* residualTrim);
        end
 
        
        
        
        % MULTIPLY increment from left onto the current estimate.
        lastXi = xi;
        xi = se3Log(se3Exp(upd) * se3Exp(xi));
        xi'
        
        % get mean and display
        err = mean((huberTrim.*residualTrim) .* residualTrim)

        %calcErr(c1,d1,c2,xi,K);
        pause(0.1);
        
        
        
        if(useLM)
            err - errLast
            if(err >= errLast)
                lambda = lambda * 5
                xi = lastXi;
                
                if(lambda > 5)
                    break;
                end
            else
                lambda = lambda /1.5
            end
        end
        
        if(useGN || useGD)
            if(err / errLast > 0.995)
                break;
            end
        end
            
        errLast = err;
    end
    
    lvl
end
