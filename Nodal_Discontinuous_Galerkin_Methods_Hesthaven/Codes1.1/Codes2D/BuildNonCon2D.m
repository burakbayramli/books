function [neighbors] = BuildNonCon2D(NGauss, tol)

% function [neighbors] = BuildNonCon2D(NGauss, tol)
% Purpose: find element to element connections through non-conforming interfaces
%          (** elements assumed straight sided **)

Globals2D;

% 1. Build Gauss nodes
[gz, gw] = JacobiGQ(0, 0, NGauss-1);

% 1.1 Find location of vertices of boundary faces
vx1 = VX(EToV(:,[1,2,3])); vx2 = VX(EToV(:,[2,3,1]));
vy1 = VY(EToV(:,[1,2,3])); vy2 = VY(EToV(:,[2,3,1]));

idB = find(EToE==((1:K)'*ones(1,Nfaces)));
x1 = vx1(idB)'; y1 = vy1(idB)';
x2 = vx2(idB)'; y2 = vy2(idB)';

% 1.2 Find those element-faces that are on boundary faces
[elmtsB,facesB] = find(EToE==((1:K)'*ones(1,Nfaces)));
Nbc = length(elmtsB);

sk = 1;
% 2.1 For each boundary face
for b1=1:Nbc 
  % 2.2 Find element and face of this boundary face
  k1 = elmtsB(b1); f1 = facesB(b1);

  % 2.3 Find end coordinates of b1'th boundary face
  x11 = x1(b1);  y11 = y1(b1);  x12 = x2(b1);  y12 = y2(b1);
  
  % 2.4 Compute areas, lengths and face coordinates used in intersection 
  % tests comparing b1'th boundary face with all boundary faces
  area1 = abs((x12-x11)*(y1-y11) - (y12-y11)*(x1-x11)); %scale
  area2 = abs((x12-x11)*(y2-y11) - (y12-y11)*(x2-x11));
  L   = (x12-x11)^2 + (y12-y11)^2 ; 
  r21 = ((2*x1-x11-x12)*(x12-x11) + (2*y1-y11-y12)*(y12-y11))/L;
  r22 = ((2*x2-x11-x12)*(x12-x11) + (2*y2-y11-y12)*(y12-y11))/L;

  % 2.5 Find range of local face coordinate (bracketed between -1 and 1)
  r1 = max(-1,min(r21,r22)); r2 = min(1,max(r21,r22));

  % 2.6 Compute flag for overlap of b1 face with all other boundary faces
  flag = area1+area2+(r1<= -1 & r2<= -1)+(r1>=1 & r2>=1)+(r2-r1<tol);

  % 2.7 Find other faces with partial matches
  matches = setdiff(find(flag < tol),b1); 
  Nmatches = length(matches(:));

  if(Nmatches>0)
    % 3.1 Find matches
    r1 = r1(matches); r2 = r2(matches); 

    % 3.2 Find end points of boundary-boundary intersections
    xy11 = 0.5*[x11;y11]*(1-r1) +  0.5*[x12;y12]*(1+r1);
    xy12 = 0.5*[x11;y11]*(1-r2) +  0.5*[x12;y12]*(1+r2);

    % 3.3 For each face-face match
    for n=1:Nmatches

      % 3.4 Store which elements intersect
      k2 = elmtsB(matches(n)); f2 = facesB(matches(n));
      neighbors{sk}.elmtM = k1; neighbors{sk}.faceM = f1;
      neighbors{sk}.elmtP = k2; neighbors{sk}.faceP = f2;

      % 3.5 Build physical Gauss nodes on face fragment
      xg = 0.5*(1-gz)*xy11(1,n) + 0.5*(1+gz)*xy12(1,n);
      yg = 0.5*(1-gz)*xy11(2,n) + 0.5*(1+gz)*xy12(2,n);

      % 3.6 Find local coordinates of Gauss nodes
      [rg1,sg1] = FindLocalCoords2D(k1, xg, yg);
      [rg2,sg2] = FindLocalCoords2D(k2, xg, yg);

      % 3.7 Build interpolation matrices for volume nodes ->Gauss nodes
      gVM = InterpMatrix2D(rg1,sg1); neighbors{sk}.gVM  = gVM;
      gVP = InterpMatrix2D(rg2,sg2); neighbors{sk}.gVP  = gVP;

      % 3.8 Find face normal 
      neighbors{sk}.nx = nx(1+(f1-1)*Nfp,k1);
      neighbors{sk}.ny = ny(1+(f1-1)*Nfp,k1);
      
      % 4.0 Build partial face data lift operator

      % 4.1 Compute weights for lifting
      partsJ = sqrt( (xy11(1,n)-xy12(1,n))^2 + (xy11(2,n)-xy12(2,n))^2 )/2;
      dgw = gw*partsJ/J(1,k1); 
        
      % 4.2 Build matrix to lift Gauss data to volume data
      neighbors{sk}.lift = V*V'*(gVM')*diag(dgw);

      sk = sk+1;
    end
  end
end
return
