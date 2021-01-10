
% function []=stitch( g_ni, g_nj, num_procs)
%  - Stitches together the pieces of the bmp files from a parallel run
%  - Assumes partitioning only in y direction
function []=stitch( g_ni, g_nj, num_procs, start_frame, end_frame)

  subs = 0;

  ni = g_ni;

  NumLayersOnRoot = mod( g_nj, num_procs);

  if( NumLayersOnRoot ~= 0)
    NumLayersPerProc = ( g_nj - NumLayersOnRoot) / ( num_procs-1);
  else
    NumLayersPerProc = ( g_nj - NumLayersOnRoot) / (num_procs);
    NumLayersOnRoot = NumLayersPerProc;
  end

  g_rho = zeros(g_nj,g_ni,3);

  for frame=start_frame:end_frame

    % Root proc handled separately
    nj = NumLayersOnRoot
    g_sj = 0;
    g_ej = 0 + NumLayersOnRoot - 1;
    g_StartNode = 0;
    filename = sprintf('rho%dx%d_frame%04d_subs%02d_proc%04d.bmp' ...
             , ni ...
             , nj ...
             , frame ...
             , subs ...
             , 0);
    rho = imread(filename,'bmp');
     figure; image(rho);
     size(g_rho)
     size(  rho)
    g_rho(g_nj+1-(g_sj+1:g_ej+1),:,1) = flipud(rho(:,:,1));
    g_rho(g_nj+1-(g_sj+1:g_ej+1),:,2) = flipud(rho(:,:,2));
    g_rho(g_nj+1-(g_sj+1:g_ej+1),:,3) = flipud(rho(:,:,3));

    % The rest of the procs
    for n=1:num_procs-1

      nj = NumLayersPerProc;
      g_sj = NumLayersOnRoot + NumLayersPerProc*(n-1);
      g_ej = NumLayersOnRoot + NumLayersPerProc*(n-1) + NumLayersPerProc - 1;

      g_StartNode = g_sj*ni;

      filename = sprintf('rho%dx%d_frame%04d_subs%02d_proc%04d.bmp' ...
               , ni ...
               , nj ...
               , frame ...
               , subs ...
               , n);
      rho = imread(filename,'bmp');
      %figure; image(rho);
      g_rho(g_nj+1-(g_sj+1:g_ej+1),:,1) = flipud(rho(:,:,1));
      g_rho(g_nj+1-(g_sj+1:g_ej+1),:,2) = flipud(rho(:,:,2));
      g_rho(g_nj+1-(g_sj+1:g_ej+1),:,3) = flipud(rho(:,:,3));
    end

    g_rho

    if max(max(g_rho(:,:,1))) ~= 0
      g_rho(:,:,1) = g_rho(:,:,1) / 255;%max(max(g_rho(:,:,1)));
    end
    if max(max(g_rho(:,:,2))) ~= 0
      g_rho(:,:,2) = g_rho(:,:,2) / 255;%max(max(g_rho(:,:,2)));
    end
    if max(max(g_rho(:,:,3))) ~= 0
      g_rho(:,:,3) = g_rho(:,:,3) / 255;%max(max(g_rho(:,:,3)));
    end

    figure; image(g_rho); axis image;

    filename = sprintf('g_rho%dx%d_frame%04d_subs%02d.bmp' ...
             , g_ni ...
             , g_nj ...
             , frame ...
             , subs );
    imwrite(g_rho,filename,'bmp');

  end



end
