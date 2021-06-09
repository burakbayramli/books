%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Script for EFR 13.4b
%We deploy the nodes in three stages
%Stage 1:  Outside the circle of radius 1, center (0,0), squarelike grid
%with gap size s = 0.2
%We can do boundary and interior nodes together:
count=1;
for xt=-1:.2:2
    for yt=-2:.2:1
        pt=[xt yt]; %test point
        if norm(pt,2)>.8+.1 & ~(xt>0 & yt<0)
            %these conditions ensure the test point is in the domain and a
            %safe distance from the boundary of the outer circle of Stage 2
            x(count)=xt;, y(count)=yt;, count=count+1;
        end
    end
end
%Stage 2:  Put nodes on concentric circles with exponential decay of radii
angles=0:pi/16:3*pi/2; %this vector of angles will not change in the loop
for k=1:40
    r=.8^k;
    for theta = angles
        x(count)=r*cos(theta);, y(count)=r*sin(theta);, count=count+1;
        if k==0 & (x(count-1)<-.95|y(count-1)>.95)
        count=count-1;, end %discard points too close to domain boundary
    end
end
%Stage 3:  Put nodes on the inside of the last circle of Stage 2
gap=3*pi/4*r/13; %approx. gap size gotton by dividing arclength of last circle 
%by number of nodes that were put on it
xvec=linspace(-r,r,2*ceil(r/gap)+1);, yvec=xvec;
for xt=xvec
    for yt=yvec
     pt=[xt yt]; %test point
        if norm(pt,2)<=r-gap/2 & ~(xt>0 & yt<0)
            %these conditions ensure the test point is in the domain and a
            %safe distance from the boundary of the circle
            x(count)=xt;, y(count)=yt;, count=count+1;
        end 
    end
end
%plot(x,y,'rp')
tri = delaunay(x,y);
hold on, trimesh(tri,x,y), axis('equal')

% Now we put in extra ghost nodes to detect bad elements
% There are several ways to do this, we will deploy them in a 
% sufficient pattern on the ray theta = - pi/4
nnodes=count-1; %number of nodes
x(count)=1;, y(count)=-1;, count=count+1;
for k=0:40
x(count)=.8^k*cos(-pi/4);, y(count)=.8^k*sin(-pi/4);, count=count+1;
end
for k=r:-gap:gap
x(count)=k*cos(-pi/4);, y(count)=k*sin(-pi/4);, count=count+1;
end
x(count)=gap/2*cos(-pi/4);, y(count)=gap/2*sin(-pi/4);, count=count+1;
tri = delaunay(x,y);
clf, plot(x(nnodes+1:count-1), y(nnodes+1:count-1), 'rp')
hold on, trimesh(tri,x,y), axis('equal')
size(tri)
%ans =
%       2406           3
badelcount=1;
for ell=1:2406
    if max(ismember(nnodes+1:count-1, tri(ell,:)))
        badel(badelcount)=ell;
        badelcount=badelcount+1;
    end
end
clf
tri=tri(setdiff(1:2406,badel),:);
x=x(1:nnodes);, y=y(1:nnodes);
trimesh(tri,x,y), axis('equal')