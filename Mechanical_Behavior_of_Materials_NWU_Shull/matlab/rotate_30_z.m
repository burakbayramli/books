sig=[4,3,0; 3,1,2; 0,2,6]*10^6;
phi=30; 
theta=[phi ,90-phi ,90;90+phi,phi ,90;90,90,0];  % this is for rotation about z axis
sigp=zeros(3); % initialize sigp to zero
for i=1:3
    for j=1:3
        for k=1:3
            for l=1:3
                sigp(i,j)=sigp(i,j)+cosd(theta(i,k))*cosd(theta(j,l))*sig(k,l);
            end
        end
    end
end
sigp %display the transformed tensor components
