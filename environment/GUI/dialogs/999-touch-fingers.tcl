
# This file is an extension for the ACT-R Environment to show
# the location of the fingers from the touch device from ACT-Touch
# (included in the extras) in the visible virtual windows using the 
# agi handlers mechanism.


proc touch_agi_change {cmd win coords params} {

  if {$win != ""  && [$win.can gettags all] != ""} {
    $win.can raise finger_spot all
    $win.can raise finger_label all
  }
}

add_agi_handler agi_change touch_agi_change


proc touch_finger_spots {cmd win coords params} {

  $win.can delete "[lindex $params 1]_[lindex $params 2]"
  set x [lindex $params 3]
  set y [lindex $params 4]
  set z [lindex $params 5]
  set name [string toupper "[string index [lindex $params 1] 0][string index [lindex $params 2] 0]"]
 
  if { $x >= [lindex $coords 0] && $x <= [expr [lindex $coords 0] + [lindex $coords 2]] && $y >= [lindex $coords 1] && $y <= [expr [lindex $coords 1] + [lindex $coords 3]]} {
    if {$z == 1} { # up
      $win.can create oval [expr $x - 10 - [lindex $coords 0]] [expr $y - 10 - [lindex $coords 1]] [expr $x + 10 - [lindex $coords 0]] [expr $y + 10 - [lindex $coords 1]] -outline brown -width 2 -fill white -tags [list "[lindex $params 1]_[lindex $params 2]" finger_spot]
      $win.can create text [expr $x - [lindex $coords 0]] [expr $y  - [lindex $coords 1]] -text $name -anchor center -fill brown -font menu_font -tags [list "[lindex $params 1]_[lindex $params 2]" finger_label]
    } else {
      $win.can create oval [expr $x - 10 - [lindex $coords 0]] [expr $y - 10 - [lindex $coords 1]] [expr $x + 10 - [lindex $coords 0]] [expr $y + 10 - [lindex $coords 1]] -outline brown -fill brown -tags [list "[lindex $params 1]_[lindex $params 2]" finger_spot]
      $win.can create text [expr $x - [lindex $coords 0]] [expr $y  - [lindex $coords 1]] -text $name -anchor center -fill white -font menu_font -tags [list "[lindex $params 1]_[lindex $params 2]" finger_label]
    }
  }
}

add_agi_handler showfinger touch_finger_spots


proc touch_removefinger {cmd win coords params} {
  $win.can delete "[lindex $params 0]_[lindex $params 1]"
}

add_agi_handler removefinger touch_removefinger

