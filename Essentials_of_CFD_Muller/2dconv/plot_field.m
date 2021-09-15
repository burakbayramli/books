function plot_field ( u, NC, mode )
  x = zeros( NC+1, NC+1 ) ;
  y = zeros( NC+1, NC+1 ) ;
  %% fill coordinate and temperature fields. The grid node
  %% numbered i,j is to the bottom left (south west) of
  %% the cell centre numbered i,j.
  for i = 1:NC+1
    for j = 1:NC+1
      x(i,j) = (i-1)/NC ;
      y(i,j) = (j-1)/NC ;


    end
  end
  uu = zeros(NC+1,NC+1) ;
  for i = 1:NC
    for j = 1:NC
      uu(i,j) = uu(i,j) + u(i,j) ;
      uu(i+1,j) = uu(i+1,j) + u(i,j) ;
      uu(i,j+1)= uu(i,j+1) + u(i,j) ;
      uu(i+1,j+1) = uu(i+1,j+1) + u(i,j) ;
    end
  end
%% internal grid nodes received 4 contributions.
  for i = 2:NC
    for j = 2:NC
      uu(i,j) = uu(i,j)/4 ;
    end
  end
%% edge grid nodes received 2 contributions
for i = 2:NC
  uu(i,1) = uu(i,1)/2 ; % bottom
  uu(i, NC+1) = uu(i,NC+1)/2 ; % top
end
for j = 2:NC
  uu(1,j) = uu(1, j)/2
  ; % left
  uu(NC+1, j) = uu(NC+1,j)/2 ; % right
end
if mode == 2
  %% mesh plot
  mesh ( x(:,1), y(1,:), uu ) ;
elseif mode == 3
  %% target profile
  yt = [ 0, .25, .25, .75, .75, 1. ] ;
  ut = [ 0. 0., 1.0, 1.0, 0., 0. ] ;
  plot ( y(NC,:), uu(NC,:), '-x', yt, ut, '-*' ) ;
  axis ( [-.2, 1.2, -.2, 1.2], "square" ) ;
  legend ( 'u|x=1', 'exact|x=1' )
  title ( 'Profiles of simulation and exact solution at x=1' )
else

  lvl = linspace(0,1,11) ;
  contourf ( x, y, uu, lvl ) ;
  axis equal ;
  title ( 'Passive scalar u at grid nodes' ) ;
endif
end

