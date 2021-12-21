%  Point source method for interior/exterior Laplace problem
clear;
close all;
format compact

%% Numbers of sources and collocation points
nPts = 	200;      % number of point sources
nCol = nPts;      % number of collocation points (if nCol>nPts, have overdetermined system)

shape = 'square'; % square or circ

%% Parameters for square problems
side = 5;         % side length for square
sideSrc = 5.001;  % side length for source locations square (>side for interior problem)

%% Parameters for circle problems
rad = 5;
radSrc = 5.1;

%% Set up locations of sources and collocation points
switch shape
    case 'square'
        V = [side side;-side side;-side -side;side -side;side side]./2;
        
        % Ensure nPts a multiple of 4
        nPts = round(nPts/4) * 4;
        nCol = nPts;
        
        % Source locations equally spaced on square of side "sideSrc"
        nSide = nPts/4;
        pts = linspace(-sideSrc,sideSrc,nSide+1)./2;
        pts = [-pts+1i*sideSrc/2, -sideSrc/2 - 1i*(pts(2:end-1)),pts-1i*sideSrc/2, ...
            sideSrc/2 + 1i*(pts(2:end-1))];
        
        % Collocation points equally spaced on square of side "side"
        nSide = nCol/4;
        cornCol = side/2;
        c = linspace(-cornCol,cornCol,nSide+1);
        c = [-c+1i*cornCol, -cornCol - 1i*(c(2:end-1)),c-1i*cornCol, ...
            cornCol + 1i*(c(2:end-1))];
        
    case 'circ'
        % Source locations equally spaced on circle of radius "radSrc"
        pts = radSrc * exp(2i*pi*(1:nPts)/nPts);
        
        % Collocation points equally spaced on circle of radisu "rad"
        c = rad * exp(2i*pi*(1:nCol)/nCol);
end

%% Plot source and collocation locations
plot(pts,'o')
hold on
plot(c,'o')
axis image
legend('Sources','Test points')

%% Assembly system matrix A
A = zeros(nCol,nPts);
for i=1:nCol
    for j=1:nPts
        A(i,j) = (-1/(2 * pi)) * log(abs(c(i)-pts(j))); % Green's function evaluations
    end
end

%% Establish boundary conditions and hence RHS
RHS = 2*ones(nCol,1);
RHS(1:nCol/2+1)=1;

%% Solve system for weights (alphas)
alpha = A\RHS;

%% Evaluate field on boundary by calculating sum of weights x sources
nEval = 2000;

switch shape
    case 'square'
        pEval = linspace(-cornCol,cornCol,nEval/4+1);
        pEval = [pEval+1i*cornCol, -cornCol + 1i*(pEval(2:end-1)), pEval-1i*cornCol, ...
            cornCol + 1i*(pEval(2:end-1))];
    case 'circ'
        pEval = rad * exp(2i*pi*(1:nEval)/nEval);
end

bdy = zeros(nEval,1);
for i=1:nEval
    solTemp = 0;
    for j=1:nPts
        solTemp = solTemp + alpha(j)*(-1/(2*pi))*log(abs(pEval(i)-pts(j)));
    end
    bdy(i) = solTemp;
end

figure
plot(bdy)
fprintf('Condition number = %e \n',cond(A));

%% Evaluate field in domain
nDom=101;
switch shape
    case 'square'
        xDom = linspace(-2.5,2.5,nDom);
    case 'circ'
        xDom = linspace(-rad,rad,nDom);
end
yDom = xDom;
[X,Y] = meshgrid(xDom,yDom);

field = zeros(nDom,nDom);
for i=1:nDom
    for j=1:nDom
        solTemp=0;
        ha = X(i,j)+1i*Y(i,j);
        for k=1:nPts
            solTemp = solTemp + alpha(k)*(-1/(2*pi))*log(abs(ha-pts(k)));
        end
        field(i,j) =solTemp;
    end
end
% Plot field
figure
pcolor(xDom,yDom,field)
hold on
plot([c c(1)],'-k','LineWidth',2) % concatenate 1st point on end to join up final 2 points
colorbar
colormap(hot)
shading interp
axis image



