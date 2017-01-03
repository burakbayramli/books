function s = matchLandmarks(x,y,SobOrder,sigmaV,lambda)

% Landmark matching algorithm
% s = matchLandmarks(x,y,k,a,lambda);
% x,y : template and target (size 2*n)
% k,a : parameters for operator L = (I-a^2*Delta)^k
% lambda : balance between energy and matching

% convert to 3D data
if size(x,1) < 3
    x(3,1) = 0;
    y(3,1) = 0;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%        Set parameters         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nx = size(x,2);           % number of template points

transmatrix = eye(3);       % additional affine transformation
transvector = zeros(3,1);   % additional affine transformation

method = 'landmarks';

gammaR = 1/lambda;

rigidmatching = 0;
elasticmatching = 1;
T = 30;                     % time discretization
resetmode = 1;              % if 1 then reset trajectories and moments

elapsedtime = 0;

useoptim = @conjgrad;      % optimization method ('fixedesc', 'adaptdesc' or 'conjgrad')
optim.maxiter = 1000;       % maximum number of iterations
optim.stepsize = .01;       % gradient descent step
optim.breakratio = 1e-8;    % ratio for termination criteria
optim.verbosemode = 0;      % if 1 display text information while processing

J = [];

X = 0;
mom = 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%        Initialize variables for matching process      %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tau = 1/(T-1);

% set matching functionals and gradients

    function R = matching(phix)
        R = sum((phix-y).^2);
    end

    function g = gradmatching(phix)
        g = 2*(phix-y);
    end

% prior affine transformation
xrig = transmatrix * x + repmat(transvector,1,nx);

sigmaV2 = sigmaV^2;

phix = zeros(3*nx,1);

dX = zeros(3*nx,1);
argin = zeros(nx);
eta = 0;
deta = 0;

gradformula = @compdeta;

%x = x(:);
%xrig = xrig(:);
y = y(:);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              main                   %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tic
if rigidmatching
    rigidmatch;
end
if elasticmatching
    diffeomatch;
end

%x = reshape(x,3,nx);
y = reshape(y,3,nx);
%xrig = reshape(xrig,3,nx);

% compute distance d(id,phi)
E = 0;
for k = 1:nx
    for t = 1:T-1
        E = E + (mom(1,k,t)+mom(1,k,t+1))*(X(1,k,t+1)-X(1,k,t))...
            + (mom(2,k,t)+mom(2,k,t+1))*(X(2,k,t+1)-X(2,k,t))...
            + (mom(3,k,t)+mom(3,k,t+1))*(X(3,k,t+1)-X(3,k,t));
    end
end
E = .5*E;
distIdPhi = sqrt(E);

clear argin gradformula Jcur dX deta eta optim meth s
clear functions
elapsedtime = elapsedtime + toc;
savestruct('s',who)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% scale/rotation/translation matching %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function rigidmatch
        % recenter
        meanxrig = mean(xrig,2);
        xrig = xrig - repmat(meanxrig,1,nx);
        transvector = transvector - meanxrig ;
        scalex = max(max(xrig')-min(xrig'));

        q = [1;0;0;0;meanxrig/scalex]; % first 4 digits = quaternion, last 3 = translation vector / scale

        [q,Jcur] = feval(useoptim,q,@rigidfunct,@rigidgrad,optim);
        J = [J,Jcur(:)'];

        [xrig,Rq] = comprigid(q);
        transmatrix = Rq * transmatrix;
        transvector = Rq * transvector + scalex * [q(5);q(6);q(7)];
        phix = xrig;

        function J = rigidfunct(q)
            % functional for rigid matching algorithm
            phi = comprigid(q);
            % compute matching term
            J = matching(phi(:));
        end

        function G = rigidgrad(q)
            a = q(1);
            ncross = [0,-q(4),q(3);q(4),0,-q(2);-q(3),q(2),0];
            ndot = [q(2),q(3),q(4);q(2),q(3),q(4);q(2),q(3),q(4)];
            [phi,Rq] = comprigid(q);
            % computes gradient wrt points %%%
            Gp = gradmatching(phi(:));
            Gp = reshape(Gp,3,nx);
            % computes gradient wrt rigid motion params %%%
            G = zeros(4,1);
            ncrossxrig = ncross*xrig;
            G(1) = 2 * sum(dot(a*xrig + ncrossxrig , Gp));
            G(2:4) = 2 * sum(cross(ncrossxrig,Gp) + a*cross(xrig,Gp) + (ndot*xrig).*Gp,2);
            G(5:7) = scalex * sum(Gp,2);
        end

        function [phi,Rq] = comprigid(q)
            Rq = [q(1)^2+q(2)^2-q(3)^2-q(4)^2 , 2*(q(2)*q(3)-q(1)*q(4)) , 2*(q(2)*q(4)+q(1)*q(3));...
                2*(q(2)*q(3)+q(1)*q(4)) , q(1)^2-q(2)^2+q(3)^2-q(4)^2 , 2*(q(3)*q(4)-q(1)*q(2));...
                2*(q(2)*q(4)-q(1)*q(3)) , 2*(q(3)*q(4)+q(1)*q(2)) , q(1)^2-q(2)^2-q(3)^2+q(4)^2];
            phi = Rq * xrig + scalex * repmat([q(5);q(6);q(7)],1,nx);
        end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%    diffeo matching         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function diffeomatch

        % reset trajectories and momentum
        if resetmode
            elapsedtime = 0;
            X = zeros(3*nx,T);
            X(:,1) = xrig(:);
            mom = zeros(3*nx,T);
            J = [];
        else
            X = reshape(X,3*nx,T);
            mom = reshape(mom,3*nx,T);
        end

        % temp variables
        eta = zeros(3*nx,T);
        deta = zeros(3*nx,1);

        % call to optimization routine
        [mom,J] = feval(useoptim,mom,@diffeofunct,@diffeograd,optim);

        % compute trajectories
        comptraj(mom);
        mom = reshape(mom,3,nx,T);
        X = reshape(X,3,nx,T);
        phix = X(:,:,T);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%% functional for diffeomatch %%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        function J = diffeofunct(mom)

            comptraj(mom);
            phix = X(:,T);

            % compute energy term
            E = 0;
            for k = 1:nx
                for t = 1:T-1
                    lock = 3*(k-1);
                    E = E + (mom(1+lock,t)+mom(1+lock,t+1))*(X(1+lock,t+1)-X(1+lock,t))...
                        + (mom(2+lock,t)+mom(2+lock,t+1))*(X(2+lock,t+1)-X(2+lock,t))...
                        + (mom(3+lock,t)+mom(3+lock,t+1))*(X(3+lock,t+1)-X(3+lock,t));
                end
            end
            E = .5*E;

            % compute matching term
            R = matching(phix);
            J = gammaR * E + R;

        end

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%% gradient for diffeomatch  %%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        function G = diffeograd(mom)

            comptraj(mom);
            phix = X(:,T);

            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%% computes eta at t=1
            %%% ie gradient of W^* norm wrt points
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            eta(:,T) = gradmatching(phix);

            %%%%%%%%%%%%%%%%%%%%%%%%%%
            %%% computes eta(t)
            %%%%%%%%%%%%%%%%%%%%%%%%%%

            % computes eta(t) with backward integration centered scheme

            for t=T:-1:2
                feval(gradformula,t,mom);
                eta(:,t-1)=eta(:,t) + (tau*2/sigmaV2)*deta;
                feval(gradformula,t-1,mom);
                eta(:,t-1)=eta(:,t) + (tau/sigmaV2)*deta; % factor 2 cancels
                deta(:) = 0;
            end

            G = 2*gammaR*mom + eta;
        end

    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function compdeta(t,mom)
        for m = 1:nx
            locm = 3*(m-1);
            for l = 1:nx
                locl = 3*(l-1);
                argin(m,l) = -((X(locm+1,t)-X(locl+1,t))^2 + (X(locm+2,t)-X(locl+2,t))^2 + (X(locm+3,t)-X(locl+3,t))^2)/sigmaV2;
            end
        end
        argout = dsobolev(SobOrder,argin);  %% BUILT IN KERNEL derV, do not remove this comment
        for m = 1:nx
            locm = 3*(m-1);
            for l = 1:nx
                locl = 3*(l-1);
                argout(m,l) = - argout(m,l) * ( ...
                    (eta(locl+1,t)*mom(locm+1,t) + eta(locl+2,t)*mom(locm+2,t) + eta(locl+3,t)*mom(locm+3,t)) + ...
                    (eta(1+locm,t)*mom(1+locl,t) + eta(2+locm,t)*mom(2+locl,t) + eta(3+locm,t)*mom(3+locl,t)) + ...
                    2*gammaR * (mom(1+locl,t)*mom(1+locm,t) + mom(2+locl,t)*mom(2+locm,t) + mom(3+locl,t)*mom(3+locm,t)));

                deta(1+locm) = deta(1+locm) + argout(m,l) * (X(1+locm,t)-X(1+locl,t));
                deta(2+locm) = deta(2+locm) + argout(m,l) * (X(2+locm,t)-X(2+locl,t));
                deta(3+locm) = deta(3+locm) + argout(m,l) * (X(3+locm,t)-X(3+locl,t));
            end
        end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% updates trajectories %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function comptraj(mom)
        for t = 1:T-1
            feval(@kernelsum,t,mom);
            X(:,t+1) = X(:,t) + tau * dX;
            feval(@kernelsum,t+1,mom);
            dX = .5*dX;
            X(:,t+1) = X(:,t) + tau * dX;
            dX(:) = 0;
        end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function kernelsum(t,mom)
        for m = 1:nx
            locm = 3*(m-1);
            for l = 1:nx
                locl = 3*(l-1);
                argin(m,l) = -( ...
                    (X(1+locm,t)-X(1+locl,t))^2 + ...
                    (X(2+locm,t)-X(2+locl,t))^2 + ...
                    (X(3+locm,t)-X(3+locl,t))^2)/sigmaV2;
            end
        end
        argout = sobolev(SobOrder,argin);  %% BUILT IN KERNEL kerV, do not remove this comment
        for m = 1:nx
            locm = 3*(m-1);
            for l = 1:nx
                locl = 3*(l-1);
                dX(1+locm) = dX(1+locm) + argout(m,l) * mom(1+locl,t);
                dX(2+locm) = dX(2+locm) + argout(m,l) * mom(2+locl,t);
                dX(3+locm) = dX(3+locm) + argout(m,l) * mom(3+locl,t);
            end
        end
    end


    function savestruct(sname,field)

        % save workspace variables in structure
        % savestruct('s') only updates fields which are already defined in s
        %
        % savestruct('s',field) specify the list of fields

        if nargin==1
            field = fieldnames(sname);
        end

        for n = 1:length(field)
            fld = field{n};
            if ~strcmp(sname,fld)  % prevent from saving structure inside itself..
                eval([sname,'.',fld,'=eval(''',fld,''',''eval(''''',sname,'.',fld,''''',''''0;'''')'');']);
            end
        end

    end

    function K = sobolev(k,r)

        % Kernel for operator L=(I-Laplacian)^k in R^2
        % expressed as a function of -|x-y|^2/sigma^2

        s = sqrt(-r) + 1e-100;
        order = k-1;
        K = s.^order .* besselk(order,s);

    end

    function K = dsobolev(k,r)

        % Derivative of kernel for operator L=(I-Laplacian)^k in R^2
        % expressed as a function of -|x-y|^2/sigma^2

        s = sqrt(-r) + 1e-100;
        order = k-2;
        K = .5 * s.^order .* besselk(order,s);

    end

end