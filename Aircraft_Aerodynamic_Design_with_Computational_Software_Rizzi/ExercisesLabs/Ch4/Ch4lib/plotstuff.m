%% Callback function called by slider event
  function plotstuff (h, event)
 t = linspace (0, 8*pi, 100); 
    n = get (h, 'value');
    x = n * t .* cos(t);  y = n * t .* sin(t);
    plot (x, y);  
    axis ([-100, 100, -100, 100]);
  end