function [div]=checkDivergenceFree(imax,jmax,dx,dy,u,v)

div=zeros(imax,jmax);

for i=1:imax
    for j=1:jmax
        div(i,j) = (1/dx)*(u(i,j)-u(i+1,j)) + (1/dy)*(v(i,j)-v(i,j+1)); 
    end
end

return
end