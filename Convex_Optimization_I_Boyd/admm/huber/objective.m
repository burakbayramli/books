function p = objective(z)
    p = ( 1/2*sum(huber(z)) );
end
