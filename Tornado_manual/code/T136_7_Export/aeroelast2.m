function[results,lattice,convergence,FEMresult]=aeroelast2(wingno,geo,state,mesh)

gear=0;
results=[];
latticetype=1;
structure=geo.structure;

[lattice,ref]=fLattice_setup2(geo,state,latticetype);


ok=0;
    j=0;  
    lattice0=lattice; %Baseline Lattice
    while ok==0       %  
        j=j+1;      
     
        
            %Solving aero forces
            [results]=solver9(results,state,geo,lattice,ref);
            [results]=coeff_create3(results,lattice,state,ref,geo);
            
            %Take the moment between the spars
            moment_axis=sum(structure.spars,2)/2; 
            
            %Computing forces on each strip.
            results=fStripforce5(geo,results,lattice,state,ref,moment_axis(wingno));
            
            results.CL
            
            
            figure(102)
            hold on
            plot(results.ystation(:,1),results.CL_local(:,1))
            
            %Saving iteration data
            con.iteration(j)=j;
            con.CL(j)=results.CL;
            con.CD(j)=results.CD;
            con.Cm(j)=results.Cm;        
                
   
               
            %Load the structure with this iterations aeroload.
            FEMresult=FEMsolver(100,geo,lattice,state,ref,mesh,results,wingno);

            %Deflect the virgin lattice of this wing, with this iterations load
            L2=fLattransform2(wingno,geo,lattice0,FEMresult,structure,mesh);
        
            [a b c]=size(L2.COLLOC);

            lattice.COLLOC(1:a,:)=L2.COLLOC;
            lattice.VORTEX(1:a,:,:)=L2.VORTEX;
            lattice.N(1:a,:)=L2.N;
            
           
            lattice.XYZ(1:a,:,:)=L2.XYZ;
         
            figure(99)
            fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','w');
            hold on
            
            
            
        % Check convergence
        if j==10
            ok=1; %NOT CONVERGED
            disp('Aeroelast function not converged!')
            disp('Structure divergent, or limit cycled, at selected state')
            convergence=0;
            return      
        end

        %if sum(results.FEM1.Total>results.FEM1.R_el)
        %    ok=1; %NOT CONVERGED
        %    disp('Structural faliure - Elastic loop aborted')
        %    disp('Internal loads too high')
        %    convergence=0;
        %    return
        %end
        
         if j>2
                change=abs(diff(con.CL))    ; 
              if change(j-1)<0.01;
                   ok=1;   %CONVERGED
                   convergence=1;
              end
              
              if change(j-1)>3.00;
                   ok=1;   %NOT CONVERGED
                   disp('Aeroelast function not converged!')
                   disp('Structure divergent, or limit cycled, at selected state')
                   convergence=0;
                   return
              end
         end
    end
    