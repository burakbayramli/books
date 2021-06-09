%Script for EFR 13.4a
count=1;
for n=1:7
s=sqrt(pi/40)/2^n;    
len = 5*pi/2/2^n; %avg. arclength of node circular arc in Omega_n
nnodes= ceil(len/s); %number of nodes to put on each circular arc
ncirc = ceil(1/2^n/s); %number of circlular arcs w/ to put nodes on Omega_n
rads = linspace(2/2^n, 1/2^n+s/2, ncirc);%radii of circular arcs with nodes
angles = linspace(pi/6, 11*pi/6, nnodes); %angles for node deployment
%delploy nodes
for r=rads
    for theta = angles
        x(count)=r*cos(theta);, y(count)=r*sin(theta);, count=count+1;
    end
end
end
%the final portion takes a slightly different approach since we want to 
%deploy nodes throughout the whole disk (not just the annulus).  We will
%thus want the circles of deployment to have radii all the way down to s
%(gap size), but on the smaller circles we should deploy less nodes
n=8;, s=sqrt(pi/40)/2^n;
len = 5*pi/2/2^n; %avg. arclength of node circular arc in Omega_n-outer circles
nnodes= ceil(len/s); %number of nodes to put on each outer circular arc
rads = linspace(2/2^n, 0, ceil(2/2^n/s));%radii of circular arcs with nodes
%angles = linspace(pi/6, 11*pi/6, nnodes); %angles for node deployment
%delploy nodes
for r=rads
    for theta = linspace(pi/6, 11*pi/6, ceil(len/s*r/(2/2^n)))
        x(count)=r*cos(theta);, y(count)=r*sin(theta);, count=count+1;
    end
end


% Put in extra ghost nodes to detect bad elements
% There are several ways to do this, we will deploy them in a 
% sufficient pattern on the positive x-axis.
nnodes=count-1; %number of nodes (=932)
for k=0:7
x(count)=1/2^k;, y(count)=0;, count=count+1;
x(count)=.75/2^k;, y(count)=0;, count=count+1;
end
for k=rads
    if k>0
     x(count)=k;, y(count)=0;, count=count+1;
 end
end   
tri = delaunay(x,y);
plot(x(nnodes+1:count-1), y(nnodes+1:count-1), 'rp')
hold on, trimesh(tri,x,y), axis('equal')
%>> size(tri)
%ans =
%       1876           3
badelcount=1;
for ell=1:1876
    if max(ismember(nnodes+1:count-1, tri(ell,:)))
        badel(badelcount)=ell;
        badelcount=badelcount+1;
    end
end
clf
tri=tri(setdiff(1:1876,badel),:);
x=x(1:nnodes);, y=y(1:nnodes);
trimesh(tri,x(1:nnodes),y(1:nnodes)), axis('equal')