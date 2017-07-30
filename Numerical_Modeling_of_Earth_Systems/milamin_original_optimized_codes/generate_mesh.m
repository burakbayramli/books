function [GCOORD,ELEM2NODE, Point_id, Phase_id] = ...
        generate_mesh(mesh_par, second_order,...
        make_int32)
    
% input:
% mesh_par structure
%     .type: type of mesh
%            0: homogeneous box
%            1: circular inclusion
%            2: inclusion and hole
%     .no_pts_incl : number of points on boundaries
%     .radius: radius of circular inclusion
%     .ellipticity: of inclusion
%     .qangle: minimum angle allowed in delaunay triangulation
%     .area_glob: use a minimum area constraint if > 0
%
% second_order: generate six node, second order triangles
% make_int32: convert the suitable output arrays to integer (to save space)
%
% output:
%
% gcoord: coordinates of nodes, 2(dimensions) x number of nodes
% elem2node: element connectivity: number of nodes per element x number of
%                                  elements
% point_id: node id: 0:   interior 
%                    1:   boundary 
%                    100: on inclusion
%                    200: on hole
% phase_id: element id: 1: regular domain
%                       2: inclusion
%
%
% modified from:
%
%   Part of MILAMIN: MATLAB-based FEM solver for large problems, Version 1.0
%   Copyright (C) 2007, M. Dabrowski, M. Krotkiewski, D.W. Schmid
%   University of Oslo, Physics of Geological Processes
%   http://milamin.org
%   See License file for terms of use.
%

modelname = 'model';

% (mode = 'ascii')  - Triangle output written as ASCII files
% (mode = 'binary') - Triangle output written as binary files
mode='binary';
%mode='ascii';



pointlist   = [];
segmentlist = [];
regionlist  = [];
holelist    = [];

x_min = 0;
x_max = 1;
y_min = 0;
y_max = 1;

pts_l = 1;
pts_u = 0;

%BOX
BOX          = [x_min x_max x_max x_min; y_min y_min y_max y_max];
no_pts       = size(BOX,2);
pts_u        = pts_u + no_pts;
BOX_s        = [pts_l:pts_u;pts_l+1:pts_u+1; 1 1 1 1];
BOX_s(2,end)   = pts_l;
pts_l        = pts_l+no_pts;
BOX_p        = [x_min+1e-5; y_min+1e-5;1];

pointlist    = [pointlist   BOX];
segmentlist  = [segmentlist BOX_s];
regionlist   = [regionlist  BOX_p];

%CREATE Ellipse
theta        = linspace(0,2*pi,mesh_par.no_pts_incl);
theta(end)   = [];

b = 1- mesh_par.ellipticity^2;

xx           =     cos(theta);
yy           = b * sin(theta);

%
% inclusions: set point id to 100 if in hole
%
switch mesh_par.type
    case 1
        %INCLUSION
        alpha        = [0.5 0.5];
        center_x     = alpha(1)*x_max+(1-alpha(1))*x_min;
        center_y     = alpha(2)*(y_max+y_min);
        INCLUSION    = [center_x + mesh_par.radius*xx; center_y + mesh_par.radius*yy];
        no_pts       = size(INCLUSION,2);
        pts_u        = pts_u + no_pts;
        INCLUSION_s        = [pts_l:pts_u;pts_l+1:pts_u+1; 100*ones(1,no_pts)];
        INCLUSION_s(2,end) = pts_l;
        INCLUSION_p  = [center_x; center_y; 2];
        pointlist    = [pointlist   INCLUSION];
        segmentlist  = [segmentlist INCLUSION_s];
        regionlist   = [regionlist  INCLUSION_p];

    case 2
        %INCLUSION
        alpha        = [0.75 0.5];
        center_x     = alpha(1)*x_max+(1-alpha(1))*x_min;
        center_y     = alpha(2)*(y_max+y_min);
        INCLUSION    = [center_x + mesh_par.radius*xx; center_y + mesh_par.radius*yy];
        no_pts       = size(INCLUSION,2);
        pts_u        = pts_u + no_pts;
        INCLUSION_s        = [pts_l:pts_u;pts_l+1:pts_u+1; 100*ones(1,no_pts)];
        INCLUSION_s(2,end) = pts_l;
        pts_l        = pts_l+no_pts;
        INCLUSION_p  = [center_x; center_y; 2];
        pointlist    = [pointlist   INCLUSION];
        segmentlist  = [segmentlist INCLUSION_s];
        regionlist   = [regionlist  INCLUSION_p];

        %HOLE
        alpha = 0.25;
        center_x     = alpha*x_max+(1-alpha)*x_min;
        center_y     = 0.5*(y_max+y_min);
        HOLE         = [center_x + mesh_par.radius*xx; center_y + mesh_par.radius*yy];
        no_pts       = size(HOLE,2);
        pts_u        = pts_u + no_pts;
        HOLE_s       = [pts_l:pts_u;pts_l+1:pts_u+1; 200*ones(1,no_pts)];
        HOLE_s(2,end)  = pts_l;
        HOLE_p       = [center_x; center_y];
        %
        pointlist    = [pointlist   HOLE];
        segmentlist  = [segmentlist HOLE_s];
        holelist     = [holelist  HOLE_p];


end

no_pts       = size(pointlist,2);
no_seg       = size(segmentlist,2);
no_reg       = size(regionlist,2);
no_hol       = size(holelist,2);

pointlist    = [1:no_pts;pointlist];
segmentlist  = [1:no_seg;segmentlist];
regionlist   = [1:no_reg;regionlist];
holelist     = [1:no_hol;holelist];

%write triangle input file
fid     = fopen('model.poly','w');
fprintf(fid,'%d 2 0 0\n', no_pts);
fprintf(fid,'%d %15.12f %15.12f\n', pointlist);
fprintf(fid,'%d 1\n', no_seg);
fprintf(fid,'%d %d %d %d\n', segmentlist);
fprintf(fid,'%d\n',no_hol);
fprintf(fid,'%d %15.12f %15.12f\n', holelist);
fprintf(fid,'%d 0\n', no_reg);
fprintf(fid,'%d %15.12f %15.12f %d\n', regionlist);
fclose(fid);


if(strcmp(mode,'binary'))
    binary_flag = '-b';
else
    binary_flag = [];
end
if(second_order==1)
        order_flag = '-o2';
else
        order_flag = [];
end
if(mesh_par.area_glob> 0)
    area_string = num2str(mesh_par.area_glob,' -a%12.12f');
else
    area_string ='';
end

system(['triangle ',binary_flag,' ',order_flag,' -pQIq',num2str(mesh_par.qangle),...
        'A', area_string,' ',modelname,'.poly']);



if(strcmp(mode,'binary'))

    %NODES READING
    fid=fopen([modelname,'.node'], 'r');
    fseek(fid, 0, 1);
    file_size	= ftell(fid);
    fseek(fid, 0, -1);
    dummy	= fread(fid,file_size/8,'double');
    fclose(fid);

    GCOORD		= [dummy(6:4:end)';dummy(7:4:end)'];
    Point_id	= dummy(8:4:end)';

    %ELEMS READING
    fid=fopen([modelname,'.ele'], 'r');
    fseek(fid, 0, 1);
    file_size	= ftell(fid);
    fseek(fid, 0, -1);
    dummy	= fread(fid,file_size/8,'double');
    fclose(fid);
    
    if(second_order == 1)
        ELEM2NODE	= [dummy(5:8:end)';dummy(6:8:end)';dummy(7:8:end)';...
            dummy(8:8:end)';dummy(9:8:end)';dummy(10:8:end)'];
        if(make_int32 == 1)
            ELEM2NODE	= int32(ELEM2NODE);
        end
        Phase_id		= dummy(11:8:end)';
    else
        ELEM2NODE	= [dummy(5:5:end)';dummy(6:5:end)';dummy(7:5:end)'];
        if(make_int32 == 1)
            ELEM2NODE	= int32(ELEM2NODE);
        end
        Phase_id    = dummy(8:5:end)';
    end

else

    %NODES READING
    fid =fopen(strcat(modelname,'.node'),'r');
    tmp = fscanf(fid, '%d',4);
    nnod = tmp(1);
    GCOORD = fscanf(fid, '%e', [4, nnod]);
    fclose(fid);

    GCOORD(1,:)   = [];
    Point_id = GCOORD(end,:);
    GCOORD(end,:) = [];

    %ELEMS READING
    fid =fopen(strcat(modelname,'.ele'),'r');
    tmp = fscanf(fid, '%d',3);
    nel = tmp(1);
    if(second_order==1)
        ELEM2NODE = fscanf(fid, '%d',[8, nel]);
    else
        ELEM2NODE = fscanf(fid, '%d',[5, nel]);
    end
    fclose(fid);
    Phase_id  = ELEM2NODE(end,:);
    ELEM2NODE(1,:) = [];
    ELEM2NODE(end,:) = [];
    if(make_int32)
        ELEM2NODE	= int32(ELEM2NODE);
    end

end
