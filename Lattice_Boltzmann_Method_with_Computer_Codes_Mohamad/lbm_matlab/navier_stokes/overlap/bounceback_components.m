function bounced = bounceback_components(components)
% components: D2Q9 velocity indices
% bounced: corresponding bounce-back indices.

bounced = zeros(length(components),1);
for k = 1:length(components)
    if components(k) == 2
        bounced(k) = 4;
    end
    if components(k) == 3
        bounced(k) = 5;
    end
    if components(k) == 4
        bounced(k) = 2;
    end
    if components(k) == 5
        bounced(k) = 3;
    end
    if components(k) == 6
        bounced(k) = 8;
    end
    if components(k) == 7
        bounced(k) = 9;
    end
    if components(k) == 8
        bounced(k) = 6;
    end
    if components(k) == 9
        bounced(k) = 7;
    end
end
    