function res = residual ( u, NC ) ;
%% calculate the residual, i.e. the rate of
%% change or the flux divergence.
%% input
%% u: scalar field u
  %% NC: number of cells in x,y
  %% output
%% res: flux divergence/residual of u
%% Initialise the residual to zero.
res = zeros(NC,NC) ;
h = 1/NC ; % mesh width
%% compute fluxes of internal horizontal faces.
for i=1:NC
  for j=1:NC-1
    %% cell south of the interface
    i1 = i;
    j1 = j;
    %% cell north of the interface:
    i2 = i ;
    j2 = j+1;
    %% normal direction (outward for the southern cell)
    n = [0,1] ;
    %% coordinates of the upwind/southern cell
    xi = h/2 + (i1-1)/NC ;
    yi = h/2 + (j1-1)/NC ;
    %% advection speed at the upwind/southern cell
    a = [ yi, 1-xi ] ;
    %% flux
    f = a*u(i1,j1) ;
    %% flux projected onto the face
    fns = f*n'*h ;
    %% add to the left, subtract from the right for a>0.
    res(i1,j1) = res(i1,j1) + fns ;
    res(i2,j2) = res(i2,j2) - fns ;
  end
end
%% internal vertical faces
for i=1:NC-1
  for j=1:NC
    %% cell west of the interface
    i1 = i;
    j1 = j;
    %% cell east of the interface:
    i2 = i+1 ;
    j2 = j;
    %% normal direction (outward for the western cell)

    n = [1,0] ;
    %% coordinates of the upwind/southern cell
    xi = h/2 + (i1-1)/NC ;
    yi = h/2 + (j1-1)/NC ;
    %% advection speed at the upwind/southern cell
    a = [ yi, 1-xi ] ;
    %% flux
    f = a*u(i1,j1) ;
    %% flux projected onto the face
    fns = f*n'*h ;
    %% add to the left, subtract from the right for a>0.
    res(i1,j1) = res(i1,j1) + fns ;
    res(i2,j2) = res(i2,j2) - fns ;
  end
end


%% boundary condition left, x=0:
i1 = 1 ;
for j1=1:NC
  %% boundary condition
  u_bc = 0 ;
  %% normal direction (outward for the eastern cell)
  n = [-1,0] ;
  %% coordinates of the boundary face
  xi = 0 ;
  yi = h/2 + (j1-1)/NC ;
  %% advection speed at the boundary face
  a = [ yi, 1-xi ] ;
  %% flux
  f = a*u_bc ;
  %% flux projected onto the face
  fns = f*n'*h ;
  %% this adds zero, just shown for completeness
  res(i1,j1)  = res(i1,j1) + fns ;
endfor

%% boundary condition bottom
j1 = 1 ;
for i1=1:NC
  %% boundary condition
  if i1 < NC/4

    %% left quarter, 0<x<0.25, y=0:
    u_bc = 0 ;
  elseif i1 > 3*NC/4
    %% right quarter, 0.75<x<1, y=0:
    u_bc = 0 ;
  else
    %% centre half, 0.25<x<0.75, y=0:
    u_bc = 1 ;
  endif
  %% normal direction (outward for the northern cell)
  n = [0,-1] ;
  %% coordinates of the boundary face
  xi = h/2 + (i1-1)/NC ;
  yi = 0 ;
  %% advection speed at the boundary face
  a = [ yi, 1-xi ] ;
  %% flux
  f = a*u_bc ;
  %% flux projected onto the face
  fns = f*n'*h ;
  res(i1,j1) = res(i1,j1) + fns ;
endfor

%% boundary condition right, x=1:
i1 = NC ;
for j1=1:NC
  %% boundary condition: outflow, use internal value.
  u_bc = u(i1,j1) ;
  %% normal direction (outward for the western cell)
  n = [1,0] ;
  %% coordinates of the boundary face
  xi = h/2 + (i1-1)/NC ;
  yi = h/2 + (j1-1)/NC ;
  %% advection speed at the boundary face
  a = [ yi, 1-xi ] ;
  %% flux
  f = a*u_bc ;
  %% flux projected onto the face
  fns = f*n'*h ;
  res(i1,j1) = res(i1,j1) + fns ;
endfor

%% boundary condition top, y=1:
j1 = NC ;
for i1=1:NC
  %% boundary condition: outflow, use internal value.
  u_bc = u(i1,j1) ;
  %% normal direction (outward for the southern cell)
  n = [0,1] ;
  %% coordinates of the boundary face
  xi = h/2 + (i1-1)/NC ;
  yi = h/2 + (j1-1)/NC ;
  %% advection speed at the boundary face
  a = [ yi, 1-xi ] ;
  %% flux
  f = a*u(i1,j1) ;
  %% flux projected onto the face
  fns = f*n'*h ;
  res(i1,j1) = res(i1,j1) + fns ;
endfor

end
