function result = faintarrow(tail,tip,cuv,alpha)
%
% Draws 3D arrow with shaded cylindrical shaft and conical head.
%
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
% INPUTS
%    tail   column 3-vector location of tail
%    tip    column 3-vector location of tip
%    cuv    column 3D unit vector toward camera and illumination
%
a   = tip - tail;  % arrow vector
na  = norm(a);     % norm of arrow vector
cr  = .05*na;      % cone radius
sr  = cr/6;        % shaft radius
%
absa  = abs(a);
%
% Order the components of absa to select candidate normalizing vectors from
% the euclidian axis vectors 
%        [1]           [0]           [0]
% e(1) = [0]    e(2) = [1]    e(3) = [0]
%        [0]           [0]           [1]
%
if absa(1) <= absa(2)
    if absa(2) <= absa(3)
        % absa(3) is largest
        % orthogonalize e(1) and e(2) to absa
        C = [[1;0;0],[0;1;0]];
    else
        % absa(2) is largest 
        % orthogonalize e(3) and e(1) to absa
        C = [[1;0;0],[0;0;1]];
    end
elseif absa(1) <= absa(3)
    % v(3) is largest
    % orthogonalize e(2) and e(1) to v
    C = [[1;0;0],[0;1;0]];
else
    % v(1) is largest
    % orthogonalize e(3) and e(2) to v
    C = [[0;1;0],[0;0;1]];
end;
%
% Use Gram-Schmidt orthonormalization of C to get two unit vectors v and w
% normal to the arrow shaft unit vector u
%
u = a/norm(a);
v = C(:,1) - (u'*C(:,1))*u;
v = v/norm(v);
w = C(:,2) - (u'*C(:,2))*u;
w = w - (v'*w)*v;
w = w/norm(w);
%
% Shaft length equals seventy percent of arrow length
%
se = tail + .8*a; % (shaft end position)
%
% Create unit base perimeter for conical arrowhead
%
p = v;
ss = abs(v'*cuv);
for deg=5:5:360,
    rad = deg*pi/180;
    s   = sin(rad);
    c   = cos(rad);
    sh  = abs((c*v+s*w)'*cuv);
    p   = [p,(c*v+s*w)];
    ss  = [ss,sh];  % shaft shading (scalar)
end;
%
% Draw shaded cone base and tail base
%
esh = abs(u'*cuv)*[1,1,1]; % end shading (RGB row vector)
patch(se(1)+cr*p(1,:),se(2)+cr*p(2,:),se(3)+cr*p(3,:),esh,'FaceAlpha',alpha,'EdgeAlpha',alpha);
patch(tail(1)+sr*p(1,:),tail(2)+sr*p(2,:),tail(3)+sr*p(3,:),esh,'FaceAlpha',alpha,'EdgeAlpha',alpha);
%
% Draw shaded conical head and shaded cylindrical shaft
%
for k=1:72,
    sn = cross(se+.1*p(:,k)-tip(:),se+.1*p(:,k+1)-tip(:));
    sn = sn/norm(sn);          % Cone surface normal
    sh = abs(sn'*cuv)*[1,1,1]; % Cone shading
    %
    % Cone patch
    %
    patch([se(1)+cr*p(1,k),se(1)+cr*p(1,k+1),tip(1)],[se(2)+cr*p(2,k),se(2)+cr*p(2,k+1),tip(2)],[se(3)+cr*p(3,k),se(3)+cr*p(3,k+1),tip(3)],sh,'LineStyle','none','FaceAlpha',alpha);
    %
    % Shaft patch
    %
    patch([se(1)+sr*p(1,k),se(1)+sr*p(1,k+1),tail(1)+sr*p(1,k+1),tail(1)+sr*p(1,k)],[se(2)+sr*p(2,k),se(2)+sr*p(2,k+1),tail(2)+sr*p(2,k+1),tail(2)+sr*p(2,k)],[se(3)+sr*p(3,k),se(3)+sr*p(3,k+1),tail(3)+sr*p(3,k+1),tail(3)+sr*p(3,k)],ss(k)*[1,1,1],'LineStyle','none','FaceAlpha',alpha);
end;
result = 1;
return;