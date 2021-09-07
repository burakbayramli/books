
function dmx=get_cheby_matrix(nx)

% attempt to calculate derivative with Chebyshef

% initialize coordinates

    cx=(1:nx+1)*0;
    
	for ix=0:nx, x(ix+1)=cos(pi*ix/nx);  end

%  initialize derivative operators  (nx)

	cx(1)=2.;
	cx(nx+1)=2.;
	cx(2:nx)=1.;

% diagonal

	for i=0:nx,
	for j=0:nx,

	if i==j,
	if i~=0,
        if i~=nx,
	        dmx(i+1,i+1)=-x(i+1)/(2*(1-x(i+1)*x(i+1)));
        end
    end
   
	else
	dmx(i+1,j+1)=(cx(i+1)*(-1)^(i+j))/(cx(j+1)*(x(i+1)-x(j+1)));
end

end
end

%  corners

	dmx(1,1)=(2*nx*nx+1)/6;	
	dmx(nx+1,nx+1)=-dmx(1,1);
    
    
