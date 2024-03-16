function meshwing_plot (x, y, z)
  Y = [min(y(:)), max(y(:))];
  if (Y(2) - Y(1) < eps)
				  % flat
    plot (x, z, 'ko-', x', z', 'kx-') % planform
    xlabel ('x')
    ylabel ('z')
  else
				% third-angle ortho.
    subplot (2, 2, 1), plot (z, x, 'ko-', z', x', 'kx-')
    ylabel ('x')
    subplot (2, 2, 3), plot (z, y, 'ko-', z', y', 'kx-')
    xlabel ('z'), ylabel ('y')
    subplot (2, 2, 4), plot (x, y, 'ko-', x', y', 'kx-')
    xlabel ('x')
  end%if
