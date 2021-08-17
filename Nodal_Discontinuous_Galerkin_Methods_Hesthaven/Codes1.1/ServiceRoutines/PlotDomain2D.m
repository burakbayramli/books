function PlotDomain2D()

% function PlotDomain2D()
% Purpose: Show domain boundary

Globals2D;

hold on;
Nwall    = length(find(BCType(:)==Wall));
Ninflow  = length(find(BCType(:)==In));
Noutflow = length(find(BCType(:)==Out));
Ncyl     = length(find(BCType(:)==Cyl));

types = [];
sk = 1;
if(Nwall)
  plot([0,0],[0,0], 'k-');
  types{sk} = 'Wall';
  sk = sk+1;
end
if(Noutflow)
  plot([0,0],[0,0], 'k:');
  types{sk} = 'Outflow';
  sk = sk+1;
end
if(Ninflow)
  plot([0,0],[0,0], 'k--');
  types{sk} = 'Inflow';
  sk = sk+1;
end
if(Ncyl)
  plot([0,0],[0,0], 'k-.');
  types{sk} = 'Cylinder'; 
  sk = sk+1;
end

ha = legend(types);
set(ha, 'Fontsize', 16)

for k=1:K
  for f=1:Nfaces
    bc = BCType(k,f);
    ids = (k-1)*Nfp*Nfaces+(f-1)*Nfp+(1:Nfp);
    switch(bc)
      case Wall
	plot(Fx(ids), Fy(ids), 'k-');
      case In
	plot(Fx(ids), Fy(ids), 'k--');
      case Out
	plot(Fx(ids), Fy(ids), 'k:');
      case Cyl
	plot(Fx(ids), Fy(ids), 'k-.');
    end
  end
end

hold off
axis equal
xmax = max(max(x)); xmin = min(min(x));
ymax = max(max(y)); ymin = min(min(y));

Lx = xmax-xmin;
Ly = ymax-ymin;
xmax = xmax+.1*Lx; xmin = xmin-.1*Lx;
ymax = ymax+.1*Ly; ymin = ymin-.1*Ly;

axis([xmin xmax ymin ymax])
drawnow; pause(.05);
return;
