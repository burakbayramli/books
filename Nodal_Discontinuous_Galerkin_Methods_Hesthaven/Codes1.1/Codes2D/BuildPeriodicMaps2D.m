function BuildPeriodicMaps2D(xperiod, yperiod)

% function [] = BuildPeriodicMaps2D(xperiod, yperiod);
% Purpose: Connectivity and boundary tables for with all maps returned 
%          in Globals2D assuming periodicity

Globals2D;

% Find node to node connectivity
vmapM = zeros(K*Nfaces*Nfp,1); vmapP = zeros(K*Nfaces*Nfp,1);

for k1=1:K
    for f1=1:Nfaces
        k2 = EToE(k1,f1); f2 = EToF(k1,f1);
        
        vidL = Fmask(:,f1) + (k1-1)*Np; vidR = Fmask(:,f2) + (k2-1)*Np;

        fidL = (1:Nfp) + (f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
        fidR = (1:Nfp) + (f2-1)*Nfp + (k2-1)*Nfp*Nfaces;

        vmapM(fidL) = vidL; vmapP(fidL) = vidL;

        x1  = Fx(fidL')*ones(1,Nfp);  y1  = Fy(fidL')*ones(1,Nfp);
        x2  = Fx(fidR')*ones(1,Nfp);  y2  = Fy(fidR')*ones(1,Nfp);

        % Compute distance matrix
        D = (x1-x2').^2+(y1-y2').^2;

        [idL, idR] = find(abs(D)<NODETOL);
        vmapP(fidL(idL)) = vidR(idR);
    end
end

for k1=1:K
  for f1=1:Nfaces
    cx1 = sum(x(Fmask(:,f1), k1))/Nfp;
    cy1 = sum(y(Fmask(:,f1), k1))/Nfp;

    k2 = EToE(k1,f1); f2 = EToF(k1,f1);    
    if(k2==k1)
      for k2=1:K
	    if(k1~=k2)
	      for f2=1:Nfaces
	        if(EToE(k2,f2)==k2)

	          cx2 = sum(x(Fmask(:,f2), k2))/Nfp;
	          cy2 = sum(y(Fmask(:,f2), k2))/Nfp;
	      
	          dx = sqrt( (abs(cx1-cx2)-xperiod)^2 + (cy1-cy2)^2);
	          dy = sqrt( (cx1-cx2)^2 + (abs(cy1-cy2)-yperiod)^2);
	      
	          if(dx<NODETOL | dy<NODETOL)
 		        EToE(k1,f1) = k2;  EToE(k2,f2) = k1;
		        EToF(k1,f1) = f2;  EToF(k2,f2) = f1;

		        vidL = Fmask(:,f1) + (k1-1)*Np;
		        vidR = Fmask(:,f2) + (k2-1)*Np;
		
		        fidL = (1:Nfp) + (f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
		        fidR = (1:Nfp) + (f2-1)*Nfp + (k2-1)*Nfp*Nfaces;
		
		        x1  = Fx(fidL')*ones(1,Nfp);  y1  = Fy(fidL')*ones(1,Nfp);
		        x2  = Fx(fidR')*ones(1,Nfp);  y2  = Fy(fidR')*ones(1,Nfp);
		
		        % Compute distance matrix
		        if(dx<NODETOL)
		           D = (abs(x1-x2')-xperiod).^2+(y1-y2').^2;
			    else
		           D = (x1-x2').^2+(abs(y1-y2')-yperiod).^2;
			    end

		        [idL, idR] = find(abs(D)<NODETOL);
		        if(length(idL)~=Nfp)
		          disp('shot and missed')
		        end
		        vmapP(fidL(idL)) = vidR(idR); vmapP(fidR(idR)) = vidL(idL);
	          end
	        end
	      end
	    end
      end
    end
  end
end

% Create default list of boundary nodes
mapB = find(vmapP==vmapM); vmapB = vmapM(mapB);
return;
