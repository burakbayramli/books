#!/usr/bin/tclsh

proc find { func dir } {
    # visit all files in dir
    # if dir is a directory, call find again
    # if dir is a regular file, call the user-speficied
    # function func

    foreach file [ glob "$dir/*" "$dir/.*" ] {
        # prevent recursive visiting of current and parent dir:
	if { [string compare [file tail $file] "." ] != 0 && \
 	     [string compare [file tail $file] ".."] != 0 } {

	    if [ file isdirectory $file ] {
                # recurse into new directory:
		find $func $file
	    } elseif [ file isfile $file ] {
                # file is regular, call user-provided func:
		$func $file
	    } else {
		#puts "cannot treat $file"
	    }
	}
    }
}


proc writesize { file } {
    set size [ file size $file ]
    if { $size > 1000000 } { 
	puts [format "%.2fMb file in %s" \
		[expr $size/1000000.0] $file]
    }
}

find writesize "$env(HOME)"



