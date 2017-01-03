function slash = slashType(sampleDirectoryString)

%     sampleDirectoryString = pwd;
    back_slash = '\';
    forward_slash = '/';
    indices = find(       sampleDirectoryString == back_slash ...
                    |     sampleDirectoryString == forward_slash    );
    index = indices(1);
    slash = sampleDirectoryString(index);
    
    
    
