function ghandle = plotresults(s,ghandle)

% plotresults(s);    display results of landmarks matching
%  s is the output structure of a call to matchLandmarks

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% display parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showminim = 0;                  % if 1 plot functional vs iterations
showtitle = 0;                  % if 1 write parameters on figure
xcolor = [92;33;165]/256;       % color for template data
xrigcolor = [1;1;1];            % color for xrig
phicolor = [92;165;33]/256;     % color for deformation of template
ycolor = [168;30;30]/256;       % color for target data
xlegend = 'template';           % legend for template plot
ylegend = 'target';
philegend = 'elastic';
xriglegend = 'rigid';
xmarker = 'd';            % marker for template plot
ymarker = '*';
phimarker = '+';
xrigmarker = 'o';
timeflow = 1;                   % time fraction of deformation flow
showtraj = 1;                   % plot trajectories
showmomtraj = 0;                % plot momentum as arrows along trajectories
showgrid = 1;                   % display grid
showimage = 0;                  % display image
gridsize = 30;                  % size of grid
targetcolors = [1;0;0];
viewvector = 0;                 % 3D display view vector
axisvector = 0;                 % 3D display axis vector
updategraph = 0;                % only update deformed data
printag = 0;                    % if 1 save figure
printname = 's';                % file name to save figure
printmode = '-depsc';           % file type
printext = '.eps';              % file extension
printopt = '';                  % additional printing options
showlegend = 1;                 % if 1 print legend

T = s.T;
X = s.X;
x = s.x;
y = s.y;
xrig = s.xrig;
y = s.y;

if s.rigidmatching
    show = {'x','y','phi','xrig'};         % elements to be plotted: x, y, phi, xrig
else
    show = {'x','y','phi'};         % elements to be plotted: x, y, phi, xrig
end

stepflow = round(timeflow*T);
phi = X(:,:,stepflow);

clf

if showminim & isfield(s,'J')
    subplot(6,3,17)
    hold on
    if length(s.J) > 1
        plot(s.J,'k')
    end
    subplot(1.2,1,1)
end

hold on
if showtraj & length(size(X))==3
        
        ghandle.traj = plot3(squeeze(X(1,:,1:stepflow))',squeeze(X(2,:,1:stepflow))',squeeze(X(3,:,1:stepflow))');       % plot trajectories
        set(ghandle.traj,'Color',targetcolors(:,1))
end

if showmomtraj & mom~=0
    ghandle.momtraj = quiver3(squeeze(X(1,:,1:stepflow))',squeeze(X(2,:,1:stepflow))',squeeze(X(3,:,1:stepflow))',tau*squeeze(mom(1,:,1:stepflow))',tau*squeeze(mom(2,:,1:stepflow))'/10,tau*squeeze(mom(3,:,1:stepflow))'/10,'g');
end

legh = [];

for l = 1:length(show)
    clear h
    eln = show{l};
    elc = eval([eln,'color']);
        h.title = [eval([eln,'legend'])];
        
        el = eval(eln);
            h.points = plot3(el(1,:),el(2,:),el(3,:),eval([eln,'marker']));
            set(h.points,'MarkerSize',8,'Color',targetcolors(:,1),...
                'MarkerFaceColor',targetcolors(:,1),...
                'LineWidth',1.5);
            set(h.points,'Tag',h.title);
            legh = [legh,h.points];
    eval(['ghandle.',eln,' = h;'])
end

if showgrid
    
    % construction et affichage de la grille 3D
sz = 1.1 * max(max(x') - min(x'));
gridstep = sz/gridsize;
meanx = .5 * (max(x')+min(x'));
g1 = (meanx(1)-sz/2):gridstep:(meanx(1)+sz/2);
s1 = length(g1);
g2 = (meanx(2)-sz/2):gridstep:(meanx(2)+sz/2);
s2 = length(g2);
if x(3,:)
    g3 = (meanx(3)-sz/2):gridstep:(meanx(3)+sz/2);
else
    g3 = 0;
end
s3 = length(g3);
g1 = repmat(g1',1,s2*s3);
g2 = kron(g2,ones(s3,s1));
g3 = kron(ones(s1,s2),g3);
g1 = g1(:)';
g2 = g2(:)';
g3 = g3(:)';
g = flow(s,[g1;g2;g3],0:stepflow-1);
g1 = reshape(g(1,:),s1,s2*s3);
g2 = reshape(g(2,:),s1,s2*s3);
g3 = reshape(g(3,:),s1,s2*s3);
h = [];
if s1 > 1
    h = [h;plot3(g1,g2,g3,'k')];
end
g1 = g1';
g1 = reshape(g1,s3,s1*s2);
g2 = g2';
g2 = reshape(g2,s3,s1*s2);
g3 = g3';
g3 = reshape(g3,s3,s1*s2);
if s3 > 1
    h = [h;plot3(g1,g2,g3,'k')];
end
g1 = g1';
g1 = reshape(g1,s2,s1*s3);
g2 = g2';
g2 = reshape(g2,s2,s1*s3);
g3 = g3';
g3 = reshape(g3,s2,s1*s3);
if s2 > 1
    h = [h;plot3(g1,g2,g3,'k')];
end
set(h,'Color',.6*[1,1,1]);

end

if showimage
    %A = repmat(1:50,50,1)';
    A = flipud(double(imread('ess.jpg'))');
    I = repmat(A,[1,1,50]);
    C = [0,1;0,1;0,1];
    J = transport(s,I,C);
    imagesc([0,1],[0,1],flipud(J(:,:,1)'),'AlphaData',.5);
end

if showlegend & legh
    legend(legh,get(legh,'Tag'))
end

if size(viewvector) ~= [1,1]
    view(viewvector);
else
    viewvector = view;
end

axisvector = 0;
if size(axisvector) ~= [1,1]
    axis(axisvector);
else
    axisvector = axis;
end

axis equal
%axis tight

if showtitle
    if min(sigmaV) < max(sigmaV)
        sigmaVstr = [num2str(max(sigmaV)),' -> ',num2str(min(sigmaV))];
    else
        sigmaVstr = num2str(sigmaV(1));
    end
    title(['sigmaV=',sigmaVstr,', ','gammaR=',num2str(gammaR)]);
end

axis off

camlight
%shading interp

drawnow
shg

if printag
    print(printopt,printmode,[printname,printext]);
end
