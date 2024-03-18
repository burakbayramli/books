function[results]= blundbodydrag(results,geo,state,ref,JID)
%bluntbodydrag

try
    geo.body.lenght;
catch
    terror(24)
    return
end


settings=config('startup');

if state.AS==0
   state.AS=input('Airspeed, [m/s] :'); 
end

[rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
Mach=state.AS/a;
                    
if ref.S_ref==0
    ref.S_ref=input('Reference area, [m^2] :');
end

results.CD0_blunt=zldpblunt(Mach,state.ALT,geo.body,ref);


fname=strcat(JID,'-blunt');

cd(settings.odir)
    save(fname,'results','geo','state','ref')
cd(settings.hdir)
    
tdisp(' ')
tdisp(strcat(' Solution available in output/',fname))
tdisp(' ')