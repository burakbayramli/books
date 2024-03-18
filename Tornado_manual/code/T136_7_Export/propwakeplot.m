        P=geo.prop.pos(1,:)+[0.53*geo.prop.dia(1) 0 0];
        profile=P+[zeros(101,1) zeros(101,1) [-1:0.02:1]'*geo.prop.dia(1)/2*1.3]
        
        wash=propwash(lattice.prop,profile);
        figure(1702)
        plot(wash(:,1),profile(:,3))
        hold on
