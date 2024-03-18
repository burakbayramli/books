function[]=postproc(lattice,geo,ref);
settings=config('startup');
quest=questions(20);

            if isempty(quest)
      			quest=0;
      			terror(9)
   			end
switch(quest)
case 1
   close all
   
case 2
    if geo.nwing==0          %avoid crash if geometry is empty
        terror(3)
        return
    end
   geometryplot(lattice,geo,ref);
   
case 3
   resultplot(1); %Static comp.
   
case 4
   quest2=questions(21)
   switch quest2
       case 1
         resultplot(3); %Alpha sweep
       case 2
         resultplot(4); %Beta sweep
       case 3
         resultplot(6); %P sweep
       case 4
         resultplot(7); %Q sweep
       case 5
         resultplot(8); %R sweep
       case 6
         resultplot(5); %Delta sweep
       otherwise  
   end
   
   
case 5
   resultplot(15); %Unsteady data only
   
case 6
   resultplot(16); %Unsteady data, all coefficients
   
case 7
   resultplot(10); %Wing system drag estimation
   
case 8
   resultplot(17); %Blunt body drag estimation
   
case 9
   resultplot(9); %Trefftz plane analysis
   
case 10
   cd(settings.odir)
   ls
   cd(settings.hdir)
   disp(' ');
   %try
   JID=input('Which JID do to export?: ','s');
   fname=input('Which filename to save to?: ','s');
   void=texport(JID,fname,14);
   
   
    case 11
        resultplot(18)
        
   
   %end
       
 %case(17)
 %     resultplot(17);
    %*** Write drag polar breakdown results to textfile
   % cd(settings.odir) %***
   % ls %***
   % cd(settings.hdir) %***
   % disp(' '); %***
   % JID=input('Which JID to export?: ','s'); %***
   % fname=input('Which filename to save to?: ','s'); %***
   % quest=15;
   % void=texport(JID,fname,quest); %***
%case(18)
    %*** Plot total drag breakdown for each given Mach number
    %resultplot(11) %***
%case(19)
    %*** Plot drag breakdown without VI drag and zero-lift drag 
    %*** (with trim drag components) for each given Mach number
    %resultplot(12) %***

    otherwise
    return   
end


