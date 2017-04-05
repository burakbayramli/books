function [ Id ] = downscaleDepth( I, num )

    if(num<=1)
        Id = I;
        return;
    end
    Id = (I(0+(1:2:end), 0+(1:2:end)) + I(1+(1:2:end), 0+(1:2:end)) + I(0+(1:2:end), 1+(1:2:end)) + I(1+(1:2:end), 1+(1:2:end))) ./ ...
    (sign(I(0+(1:2:end), 0+(1:2:end))) + sign(I(1+(1:2:end), 0+(1:2:end))) + sign(I(0+(1:2:end), 1+(1:2:end))) + sign(I(1+(1:2:end), 1+(1:2:end))));
    Id(isnan(Id)) = 0;
    

    [Id] = downscaleDepth( Id, num -1 );

    
end

