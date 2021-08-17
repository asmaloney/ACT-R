# This file creates windows on the Environment side for showing
# Experiments generated with the AGI.
# There can be any number of experiment windows on the environment side,
# but there's only one "main" handler which isn't visible but is used
# to catch the environment closing to remove the vv-handler.

toplevel .env_window
wm withdraw .env_window

global name_to_window_array

global name_to_params_array


proc name_to_window {name} {

  global name_to_window_array
  
  if {[array names name_to_window_array -exact $name] != ""} {
    return $name_to_window_array($name)
  } else {
    return ""
  }
}

proc name_to_params {name} {

  global name_to_params_array
  
  if {[array names name_to_params_array -exact $name] != ""} {
    return $name_to_params_array($name)
  } else {
    return ""
  }
}


proc add_name_to_window {name window params} {

  global name_to_window_array

  set name_to_window_array($name) $window

  global name_to_params_array

  set name_to_params_array($name) $params
}


proc remove_name_to_window {name} {

  global name_to_window_array
  
  array unset name_to_window_array $name 

  global name_to_params_array
  
  array unset name_to_params_array $name 

}


global agi_extensions_array

proc agi_handlers {cmd} {
  global agi_extensions_array
  
  if {[array names agi_extensions_array -exact $cmd] != ""} {
    return $agi_extensions_array($cmd)
  } else {
    return ""
  }
}
  

proc call_agi_handlers {cmd win coords params} {
  foreach i [agi_handlers $cmd] {
    $i $cmd $win $coords $params
  }
}


proc add_agi_handler {cmd fn} {
  global agi_extensions_array
  
  if {[array names agi_extensions_array -exact $cmd] != ""} {
    set agi_extensions_array($cmd) [lappend $agi_extensions_array($cmd) $fn]
  } else {
    set agi_extensions_array($cmd) [list $fn]
  }
}
  




set env_handler [add_new_cmd "env_vv_handler" "process_env_window" "Environment command for handling the visible virtual windows. Do not call."]

call_act_r_command "add-virtual-window-handler" nil [list $env_handler]

bind .env_window <Destroy> {
  global $env_handler
  call_act_r_command "remove-virtual-window-handler" nil [list $env_handler]
}

proc create_env_window {title x y width height} {

  set win [toplevel [new_variable_name ".visible_virtual"]]
  wm withdraw $win

  wm title $win $title
  wm geometry $win [format "%dx%d+%d+%d" $width $height $x $y]

  canvas $win.can
  place $win.can -x 0 -y 0 -relwidth 1.0 -relheight 1.0

  add_name_to_window $title $win [list $x $y $width $height]

  # don't really close it when the close button pressed
  wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"

  # send all the keypresses and mouse button 1 clicks to ACT-R

  bind $win <KeyPress> "call_act_r_command_string_only output-key nil [list [list false [translate_key %K]]]"
  bind $win <Button-1> "call_act_r_command visible-virtual-window-mouse-click nil [list [list $title %x %y]]"

  wm deiconify $win
  focus -force $win
}

 
proc lighten_color {w c degree} {
 set max [lindex [winfo rgb $w white] 0]
 set offset [expr ($max + 1) / $degree]
 set size [string length [format "%x" $max]]                 
 set clist [winfo rgb $w $c]
 set c1 [lindex $clist 0]
 set c2 [lindex $clist 1]
 set c3 [lindex $clist 2]
 return [format "\#%0*X%0*X%0*X" \
                $size [expr ($c1 + $offset) < $max ? $c1 + $offset : $max] \
                $size [expr ($c2 + $offset) < $max ? $c2 + $offset : $max] \
                $size [expr ($c3 + $offset) < $max ? $c3 + $offset : $max]]
}

proc flip_button_view {win box_down box_up down up} {
  # assume that if one of the tagged items still exists
  # that they all do since it's single threaded

  set check [$win.can find withtag $box_up]
 
  if {$check != ""} {

      $win.can raise "$box_up" "$box_down"
      $win.can raise "$up" "$box_up"
      $win.can lower "$down" "$box_up"
  }
}


proc process_env_window {model cmd} {
  global options_array

  set win [name_to_window [lindex $cmd 1]]

  set coords [name_to_params [lindex $cmd 1]]

  call_agi_handlers [lindex $cmd 0] $win $coords [lrange $cmd 2 end]

  switch [lindex $cmd 0] {
    check {
      return $options_array(use_env_window)
    }    
    select {
      wm deiconify $win
      raise $win
      focus -force $win
    }
    close {
      $win.can delete all
      remove_name_to_window [lindex $cmd 1]
      destroy $win
      set win "" 
    }
    open {
     
      if {$win != ""} {
        wm deiconify $win
        raise $win
        focus -force $win
      } else {
        create_env_window [lindex $cmd 1] [lindex $cmd 2] [lindex $cmd 3] [lindex $cmd 4] [lindex $cmd 5]
      }  
    }
    remove {
      $win.can delete [lindex $cmd 2]
    }

    attention {
      $win.can delete "[lindex $cmd 2]-attention"
      set x [lindex $cmd 3]
      set y [lindex $cmd 4]
      
      if { $x >= [lindex $coords 0] && $x <= [expr [lindex $coords 0] + [lindex $coords 2]] && $y >= [lindex $coords 1] && $y <= [expr [lindex $coords 1] + [lindex $coords 3]]} {
        $win.can create oval [expr $x - 10 - [lindex $coords 0]] [expr $y - 10 - [lindex $coords 1]] [expr $x + 10 - [lindex $coords 0]] [expr $y + 10 - [lindex $coords 1]] -outline [translate_color [lindex $cmd 5]] -width 4 -tags [list "[lindex $cmd 2]-attention" attention]
      }
    }

    clearattention {
      $win.can delete "[lindex $cmd 2]-attention"
    }

    clearcursor {
      set tag "cursor-[lindex $cmd 2]"
      $win.can delete $tag
    }

    cursor {
      set tag "cursor-[lindex $cmd 2]"
      $win.can delete $tag
      set x [lindex $cmd 3]
      set y [lindex $cmd 4]
      $win.can create polygon $x $y $x [expr $y + 15] [expr $x + 4] [expr $y + 13] [expr $x + 7] [expr $y + 19] \
                              [expr $x + 9] [expr $y + 19] [expr $x + 7] [expr $y + 12] [expr $x + 12] [expr $y + 12] \
                              -fill white -outline [lindex $cmd 5] -tags [list $tag cursor]
    }
    line {
      $win.can create line [lindex $cmd 3] [lindex $cmd 4] \
                           [lindex $cmd 5] [lindex $cmd 6] \
                           -fill [translate_color [lindex $cmd 7]] -width 2 -tags [list [lindex $cmd 2] line]
    }
    text {
      $win.can create text [lindex $cmd 3] [lindex $cmd 4] \
                           -font [list courier [expr int(round([lindex $cmd 7] /  [tk scaling]))] roman] -fill [translate_color [lindex $cmd 6]] \
                           -text [lindex $cmd 5] -anchor nw -tags [list [lindex $cmd 2] text]
    }
    button {
      # fake buttons so that we can draw the attention and cursor over them

      set x1 [lindex $cmd 3]
      set y1 [lindex $cmd 4]
      set x2 [expr $x1 + [lindex $cmd 5]]
      set y2 [expr $y1 + [lindex $cmd 6]]
      set base_color [translate_color [lindex $cmd 8]]
      set name [lindex $cmd 2]                                                                                                     

      $win.can create rectangle $x1 $y1 $x2 $y2 \
                                -fill $base_color -outline white -width 1 -tags [list $name button "$name-box-up"]

      $win.can create rectangle $x1 $y1 $x2 $y2 \
                                -fill [lighten_color $win.can $base_color 2] -outline black -width 1 -tags [list $name button "$name-box-down"]

      $win.can create text [expr $x1 + round([lindex $cmd 5] / 2)] [expr $y1 + round([lindex $cmd 6] / 2)] \
                           -font button_font -fill black -text [lindex $cmd 7] -anchor center -tags [list $name button "$name-up"]

      $win.can create text [expr $x1 + round([lindex $cmd 5] / 2) + 1] [expr $y1 + round([lindex $cmd 6] / 2) + 1] \
                           -font button_font -fill black -text [lindex $cmd 7] -anchor center -tags [list $name button "$name-down"]

      $win.can create line $x2 $y1 $x2 $y2 -width 1 -fill black -tags [list $name button "$name-up"]
      $win.can create line $x2 $y2 $x1 $y2 -width 1 -fill black -tags [list $name button "$name-up"]
      $win.can create line $x2 $y1 $x2 $y2 -width 1 -fill white -tags [list $name button "$name-down"]
      $win.can create line $x2 $y2 $x1 $y2 -width 1 -fill white -tags [list $name button "$name-down"]

      $win.can create line [expr $x1 + 1] [expr $y1 + 1] [expr $x1 + 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-down"]
      $win.can create line [expr $x1 + 1] [expr $y1 + 1] [expr $x2 - 1] [expr $y1 + 1] -width 1 -fill darkgray -tags [list $name button "$name-down"]
      $win.can create line [expr $x2 - 1] [expr $y1 + 1] [expr $x2 - 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-up"]
      $win.can create line [expr $x2 - 1] [expr $y2 - 1] [expr $x1 + 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-up"]

      # set things in the "up" position

      $win.can raise "$name-box-up" "$name-box-down"
      $win.can raise "$name-up" "$name-box-up"
      $win.can lower "$name-down" "$name-box-up"
    }
    click {
     set name [lindex $cmd 2]

      # set things in the down position

      $win.can raise "$name-box-down" "$name-box-up"
      $win.can raise "$name-down" "$name-box-down"
      $win.can lower "$name-up" "$name-box-down"

      after 100 "flip_button_view $win \"$name-box-down\" \"$name-box-up\" \"$name-down\" \"$name-up\""
    }
  }

  call_agi_handlers agi_change $win $coords [lrange $cmd 2 end]

  if {$win != ""  && [$win.can gettags all] != ""} {
    # make sure these show over everything after any update
    $win.can raise cursor all
    $win.can raise attention all
  }
}



proc translate_key {key} {
  switch $key {
    Escape {return "esc"}
    quoteleft {return "`"}
    minus {return "-"}
    equal {return "="}
    backslash {return "\\"}
    BackSpace {return "delete"}
    bracketleft {return "\["}
    bracketright {return "\]"}
    Caps_Lock {return "caps-lock"}
    semicolon {return ";"}
    Prior {return "pageup"}
    Delete {return "forward-delete"}
    Next {return "pagedown"}
    Shift_L {return "shift"}
    comma {return ","}
    period {return "."}
    slash {return "/"}
    Shift_R {return "right-shift"}
    Up {return "up-arrow"}
    Left {return "left-arrow"}
    Down {return "down-arrow"}
    Right {return "right-arrow"}
    Control_L {return "control"}
    Win_L {return "option"}
    Alt_L {return "command"}
    Alt_R {return "right-command"}
    Win_R {return "right-option"}
    Control_R {return "right-control"}
    asterisk {return "keypad-*"}
    plus {return "keypad-+"}
    0 {return "0"}
    1 {return "1"}
    2 {return "2"}
    3 {return "3"}
    4 {return "4"}
    5 {return "5"}
    6 {return "6"}
    7 {return "7"}
    8 {return "8"}
    9 {return "9"}
    default {return $key}
  }
}


proc translate_color {color} {
  switch $color {
    black {return "black"}
    blue {return "blue"}
    brown {return "brown"}
    cyan {return "cyan"}
    dark-blue {return "blue4"}
    dark-cyan {return "cyan4"}
    dark-gray {return "gray40"}
    dark-green {return  "green4"}
    dark-magenta {return "magenta4"}
    dark-red {return "red4"}
    dark-yellow {return "yellow4"}
    gray {return "gray"}
    green {return "green"}
    light-blue {return "LightBlue"}
    light-gray {return "gray90"}
    magenta {return "magenta"}
    pink {return "pink"}
    purple {return "purple"}
    red {return "red"}
    white {return "white"}
    yellow {return "yellow"}
    default {return "black"}
  }
}
