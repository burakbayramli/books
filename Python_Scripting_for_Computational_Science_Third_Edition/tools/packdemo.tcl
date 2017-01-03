#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

#  This a free demo script illustrating the pack command in Tk.
#  Please refer to http://www.scriptics.com/plugin/contrib/packLet.html
#  for information about the author.

#  Procedure to create a combination of a button imbedded in a frame
#  This is done so that the cavity filled by the button can be
#  simulated for viewing
proc buttonFrame {parent side args} {
    global numButtons selectBg current
    incr numButtons
    set bf [frame $parent.f$numButtons -relief sunken -bd 1 -bg $selectBg]
    eval {button $bf.b -text "Object $numButtons" -command "updateInfo $bf"} $args
    pack $bf.b -padx 3 -pady 3 -expand 1
    pack $bf -padx 1 -pady 1 -fill both -side $side
    
    # Update 
    updateInfo $bf

    return $bf
}

# Simple procedure to get the pack fongiuration for a widget and
#  return a specified option.  For some reason, 'pack info' can
#  only return the entire list...
proc getPackOpt {widget option} {
    set info [pack info $widget]
    if [regexp -- "$option (\[^ \]+)" $info match value] {
	return $value
    } else {
        retrn -code error "Could not find $option for $widget"
    }
}

# Update the packing of all of the buttons
proc repack {selected} {
    global current currentLabel CURRENT normalBg selectBg

    if [winfo exists $current] {
      pack configure $current -side $CURRENT(-side)
      pack configure $current -expand $CURRENT(-expand)
      foreach opt [list -anchor -fill -padx -pady] {
         pack configure $current.b $opt $CURRENT($opt)
      }
      update
    }
    return
}

# Update the button information stored in the pack fongiruation 
#  info widgets
proc updateInfo {buttonFrame} {
    global current CURRENT currentLabel normalBg selectBg
    if [winfo exists $current] {
        $current configure -bg $normalBg
    }
    set current $buttonFrame
    set currentLabel [$buttonFrame.b cget -text]
    $current configure -bg $selectBg
    catch {unset CURRENT}
    set CURRENT(-side) [getPackOpt $buttonFrame -side]
    set CURRENT(-expand) [getPackOpt $buttonFrame -expand]
    foreach opt [list -anchor -fill -padx -pady] {
       set CURRENT($opt) [getPackOpt $buttonFrame.b $opt]
    }
    return
}

# Procedure to destroy a widget and update the display information
proc killButton {button} {
    global current bf CURRENT currentLabel
    destroy $button
    if {[winfo children $bf] != ""} {
	set current [lindex [winfo children $bf] 0]
	updateInfo $current
    } else {
	set current ""
	set currentLabel ""
	foreach opt [array names CURRENT] {
	    set CURRENT($opt) ""
	}
    }
    return
}

# Procedure to turn shrink-wrapping on and off for container
proc flipShrink {frame} {
    global isShrunk shrink middle
    if {$isShrunk} {
	# Revert to expansion and filling
	pack configure $frame -fill both
	$shrink configure -text "ShrinkWrap"
	set isShrunk 0
    } else {
	pack configure $frame -fill none
	$shrink configure -text "UnShrink"
	set isShrunk 1
    }
    return
}

# Add X resource definitions
option add *Entry.background azure2
option add *Entry.font -*-courier-medium-r-normal--14-*-*-*-m-90-iso8859-1
option add *Entry.selectBackground "antique white"
option add *Entry.width 2
option add *Label.background ivory3
option add *Label.foreground darkslateblue
option add *Button.foreground ivory3
option add *Button.background darkslateblue
option add *Button.activeBackground lightslategrey
option add *Button.activeForeground ivory1
option add *Button.padY 1

# Variable initialization
set frameHeight 300
set labelFg "darkslateblue"
set labelBg "ivory4"
set normalBg "ivory3"
set selectBg "ivory4"
set infoBg "azure3"
set numButtons 0
set current ""
set currentLabel ""
set shrinkText "ShrinkWrap"
set isShrunk 0

# Create master frame
frame .f -bg ivory3 -relief raised -bd 1
pack .f -expand 1 -fill both

# Create frames to hold command bar, buttonFrames, and info
set top [frame .f.top -relief raised -bg azure3 -bd 1]
set middle [frame .f.buttons -height $frameHeight -relief raised -bg azure2 -bd 1]
set bf [frame $middle.buttons -relief raised -bg ivory2 -bd 1]
set info [frame .f.info -relief sunken -bg $infoBg -bd 1]
pack $top -fill x
pack $middle -fill both -expand 1
pack $bf -fill both -expand 1 -anchor center
pack $info -fill x -pady 2
pack propagate $middle 0

# Command-bar
set title [label $top.label -text "packdemo.tcl" -fg darkslateblue -bg azure3]
set sTop [button $top.st -text "Spawn T" -bg azure4 -fg ivory2 -command "buttonFrame $bf top"]
set sBot [button $top.sb -text "Spawn B" -bg azure4 -fg ivory2 -command "buttonFrame $bf bottom"]
set sLeft [button $top.sl -text "Spawn L" -bg azure4 -fg ivory2 -command "buttonFrame $bf left"]
set sRight [button $top.sr -text "Spawn R" -bg azure4 -fg ivory2 -command "buttonFrame $bf right"]
set shrink [button $top.shrink -text "$shrinkText" -bg azure4 -fg ivory2 -command {flipShrink $bf}]
pack $title -side left -padx 2 -pady 2
pack $shrink $sRight $sLeft $sBot $sTop -side right -padx 2 -pady 2

# Pack configuration frame
set hdr [frame $info.header -bg $infoBg]
set data [frame $info.data -bg $infoBg]
pack $hdr $data -fill x

# Header
label $hdr.label -text "Pack Configure: " -bg $infoBg -fg $labelFg
label $hdr.current -textvariable currentLabel -font {courier 14 bold} -bg $infoBg -fg azure
button $hdr.destroy -text "Destroy" -bg azure4 -fg ivory2 -command {killButton $current}
pack $hdr.label $hdr.current -side left -padx 2 -pady 2
pack $hdr.destroy -side right -padx 2 -pady 2

# Location
set loc [frame $data.loc -bg $infoBg]
label $loc.label -text "Location:" -bg $infoBg -fg $labelFg
pack $loc.label -side left -padx 4
foreach side [list top bottom left right] {
    radiobutton $loc.$side -text "$side" -variable CURRENT(-side) -bg $infoBg -activeforeground yellow\
	-value $side -anchor w -highlightthickness 0 -selectcolor yellow -highlightbackground $infoBg\
	-activebackground $infoBg -command {repack $current}
    pack $loc.$side -side left -ipadx 8 -pady 2
}
pack $loc -fill x -padx 4 -pady 4

# Anchoring
set anchor [frame $data.anchor -bg $infoBg]
label $anchor.label -text "Anchor:" -bg $infoBg -fg $labelFg
pack $anchor.label -side left -padx 4
foreach dir [list center n s e w ne nw se sw] {
    radiobutton $anchor.$dir -text "$dir" -variable CURRENT(-anchor) -bg $infoBg -activeforeground yellow\
	-value $dir -anchor w -highlightthickness 0 -selectcolor yellow -highlightbackground $infoBg\
	-activebackground $infoBg -command {repack $current}
    pack $anchor.$dir -side left -ipadx 8 -pady 2
}
pack $anchor -fill x -padx 4 -pady 4


# Expanding, Filling, Padding
set fill [frame $data.fill -bg $infoBg]
checkbutton $fill.expand -text "Expand" -justify left -anchor w -fg $labelFg -activeforeground yellow\
	 -bg $infoBg -variable CURRENT(-expand) -selectcolor yellow -highlightthickness 0\
	 -activebackground $infoBg -command {repack $current}
pack $fill.expand -side left -fill x -padx 5 -ipadx 5

label $fill.label -text "Fill:" -bg $infoBg -fg $labelFg
pack $fill.label -side left -padx 5
foreach fillType [list none x y both] {
    radiobutton $fill.$fillType -text "$fillType" -bg $infoBg -activeforeground yellow\
	-variable CURRENT(-fill) -value $fillType -anchor w -selectcolor yellow -highlightthickness 0\
	-activebackground $infoBg -command {repack $current}
    pack $fill.$fillType -side left -ipadx 10 -pady 2
}

foreach pad [list padx pady] {
    set padFrame [frame $fill.${pad}Frame -bg $infoBg]
    set l [label $padFrame.${pad}Label -text "$pad:" -fg $labelFg -bg $infoBg]
    set e [entry $padFrame.$pad -textvariable CURRENT(-$pad) -relief sunken -bd 2 -width 2 -bg azure2]
    bind $e <Return> {repack $current}
    pack $l -side left -padx 2
    pack $e -side left -fill x -expand 1
    pack $padFrame -side left -ipadx 4 -padx 2 -pady 2 -fill x
}
pack $fill -fill x -padx 4 -pady 4


