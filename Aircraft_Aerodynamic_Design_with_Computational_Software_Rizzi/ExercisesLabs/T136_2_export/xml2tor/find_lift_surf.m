function winglist = find_lift_surf(aircraft)
% Tornado function: make list winglist of lifting surfaces in aircraft
% Input
%   aircraft    CEASIOM aircraft geo struct
%
% Output
%   winglist    list winglist of lifting surfaces
%
% calls
%   --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%
list1    = fieldnames(aircraft);
winglist = {};
wingno   = 0;
for k = 1:length(list1)
    name = list1{k};

    if strncmpi(name,'fuse',4); % make a cruciform body as two wings
        cc = double(name(5));
        if (cc <= 57) && (cc >=  48) % fuse 1 ... fuse 9
            wingno = wingno+1;
            winglist{end+1}=aircraft.(name);
            winglist{end}.type = 'fuseH';
            winglist{end}.seqno = wingno;
            winglist{end}.name  = name;
            %            disp(['Fuselage XH: ',name])
            wingno = wingno+1;
            winglist{end+1}=aircraft.(name);
            winglist{end}.type = 'fuseV';
            winglist{end}.seqno = wingno;
            winglist{end}.name  = name;
            %            disp(['Fuselage XV: ',name])
        end

    elseif strncmp(name,'Wing',4)
        cc = double(name(5));
        if (cc <= 57) && (cc >=  48) % wing1 ... wing9
            if aircraft.(name).area > 0.001
                wingno              = wingno+1;
                winglist{end+1}     = aircraft.(name);
                winglist{end}.type  = 'wing';
                winglist{end}.seqno = wingno;
                winglist{end}.name  = name;
                %               disp(['Wing: ',name])
            end
        end

    elseif strncmp(name,'Horizontal_tail',15)
        if aircraft.(name).area > 0.001
            wingno = wingno + 1;
            winglist{end+1}=aircraft.(name);
            winglist{end}.type = 'Htail';
            winglist{end}.seqno = wingno;
            winglist{end}.name  = name;
            %            disp(['Hor Tail: ',name])
        end

    elseif strncmp(name,'Vertical_tail',13)
        if aircraft.(name).area > 0.001
            wingno = wingno + 1;
            winglist{end+1}=aircraft.(name);
            winglist{end}.type = 'Vtail';
            winglist{end}.seqno = wingno;
            winglist{end}.name  = name;
            %            disp(['Ver Tail: ',name])
        end

    elseif strncmp(name,'Canard',6)
        if aircraft.(name).area > 0.001
            wingno = wingno + 1;
            winglist{end+1}=aircraft.(name);
            winglist{end}.type = 'Canard';
            winglist{end}.seqno = wingno;
            winglist{end}.name  = name;
            %            disp(['Canard: ',name])
        end
    end
end
end