K = [517.3 0 318.6;	0 516.5 255.3; 0 0 1];
c2 = double(imreadbw('rgb/1305031102.175304.png'));
c1 = double(imreadbw('rgb/1305031102.275326.png'));


d2 = double(imread('depth/1305031102.160407.png'))/5000;
d1 = double(imread('depth/1305031102.262886.png'))/5000;

% result:
% approximately -0.0021    0.0057    0.0374   -0.0292   -0.0183   -0.0009

%%
K = [ 535.4  0 320.1;	0 539.2 247.6; 0 0 1];
c1 = double(imreadbw('rgb/1341847980.722988.png'));
c2 = double(imreadbw('rgb/1341847982.998783.png'));


d1 = double(imread('depth/1341847980.723020.png'))/5000;
d2 = double(imread('depth/1341847982.998830.png'))/5000;

% result:
%  approximately -0.2894 0.0097 -0.0439  0.0039 0.0959 0.0423


%%

% initialization
xi = [0 0 0 0 0 0]';

% pyramid levels
for lvl = 5:-1:1
    % get downscaled image, depth image, and K-matrix of down-scaled image.
    [IRef, Klvl] = downscaleImage(c1,K,lvl);
    I = downscaleImage(c2,K,lvl);
    [DRef] = downscaleDepth(d1,lvl);

    % just do at most 20 steps.
    errLast = 1e10;
    for i=1:10
        
        % calculate Jacobian of residual function (Matrix of dim (width*height) x 6)
        %[Jac, residual] = deriveErrNumeric(IRef,DRef,I,xi,Klvl);   % ENABLE ME FOR NUMERIC DERIVATIVES
        [Jac, residual] = deriveErrAnalytic(IRef,DRef,I,xi,Klvl);   % ENABLE ME FOR ANALYTIC DERIVATIVES
        
        % just take the pixels that have no NaN (e.g. because
        % out-of-bounds, or because the didnt have valid depth).
        valid = ~isnan(sum(Jac,2));
        residualTrim = residual(valid,:);
        JacTrim = Jac(valid,:);

        % do Gauss-Newton step
        upd = - (JacTrim' * JacTrim)^-1 * JacTrim' * residualTrim;
        
        % MULTIPLY increment from left onto the current estimate.
        xi = se3Log(se3Exp(upd) * se3Exp(xi))
        
        % get mean and display
        err = mean(residualTrim .* residualTrim)

        
        %calcErr(c1,d1,c2,xi,K);
        pause(0.1);
        
        % break if no improvement
        if(err / errLast > 0.99)
            break;
        end
        errLast = err;
    end
    
    lvl
end
