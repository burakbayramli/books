function [ys,chords] = lattic2chord(lattice,geo)
xyz   = lattice.XYZ;
nwing = geo.nwing;
ny    = geo.ny;
nx    = geo.nx+geo.fnx;
nelem = geo.nelem;
symm  = geo.symetric;
chords = cell(nwing,1);
for k = 1:nwing
    ip = 0;
    id = 1;
    nc = sum(ny(k,:))*(symm(k)+1);
    ys = zeros(nc,1);
    ch = zeros(nc,1);
    dy = zeros(nc,1);
    for j = 1:nelem(k)
        nxx = nx(k,j);
        nyy = ny(k,j);
        for i = 1:nyy
            le = squeeze((xyz(id,1,:)+xyz(id,2,:))/2);
            te = squeeze((xyz(id+nxx-1,3,:)+xyz(id+nxx-1,4,:))/2);
  %          plot3([le(1),te(1)],[le(2),te(2)],[le(3),te(3)])
            id = id+nxx;
  %          hold on
            ch(i+ip)=norm(te-le);
            ys(i+ip) = le(2);
        end
        ip = ip+nyy;
        if symm(k)
            for i = 1:nyy
                le = squeeze((xyz(id,1,:)+xyz(id,2,:))/2);
                te = squeeze((xyz(id+nxx-1,3,:)+xyz(id+nxx-1,4,:))/2);
 %               plot3([le(1),te(1)],[le(2),te(2)],[le(3),te(3)])
 %           hold on
            id = id+nxx;
                ch(i+ip)=norm(te-le);
                ys(i+ip)=le(2);
            end
            ip = ip+nyy;
        end
    end
    [ys,jj]  = sort(ys);
    chords{k}= ch(jj);
end