    IF Help='Y' THEN
     BEGIN
       WriteLn( SGP4File, ' ------------------After INITL  :--------------- ':84 );
       WriteLn( SGP4File,  '    Inputs : ' );
       WriteLn( SGP4File, 'Satn':7,Satn:13,'Yr':7,' ':13,'Ecco':7,Ecco:15:9,'Epoch':7,EPOCH:15:9,'Inclo':7,
                            Inclo:15:9 );
       WriteLn( SGP4File,  '    In/Out : ' );
       WriteLn( SGP4File, 'No':7,No:15:9 );
       WriteLn( SGP4File,  '    Outputs : ' );
       WriteLn( SGP4File, 'Method':7,Method:13,'Ainv':7,AINV:15:9,'Ao':7,AO:15:9,'Con41':7,CON41:15:9,
                           'Con42':7,CON42:15:9,'Cosio':7,COSIO:15:9 );
       WriteLn( SGP4File, 'Cosio2':7,COSIO2:15:9 );
       WriteLn( SGP4File, 'Einv':7,EINV:15:9,'Eccsq':7,EccSQ:15:9,'Omeosq':7,OMEOSQ:15:9,'posq':7,POSQ:15:9,
                           'rp':7,rp:15:9,'Rteosq':7,RTEOSQ:15:9 );
       WriteLn( SGP4File, 'Sinio':7,SINIO:15:9,'GSTo':7,GSTo:15:9 );
     END;


