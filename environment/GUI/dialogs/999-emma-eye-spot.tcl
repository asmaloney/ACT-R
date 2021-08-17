
# This file is an extension for the ACT-R Environment to show
# the location of the eye spot from the EMMA module in extras
# in the visible virtual windows using the agi handlers 
# mechanism.


proc emma_agi_change {cmd win coords params} {

  if {$win != ""  && [$win.can gettags all] != ""} {
    $win.can raise eyeloc all
  }
}

add_agi_handler agi_change emma_agi_change


proc emma_eyeloc {cmd win coords params} {
  $win.can delete "[lindex $params 0]-eyeloc"
  set x [lindex $params 1]
  set y [lindex $params 2]
     
  if { $x >= [lindex $coords 0] && $x <= [expr [lindex $coords 0] + [lindex $coords 2]] && $y >= [lindex $coords 1] && $y <= [expr [lindex $coords 1] + [lindex $coords 3]]} {
    $win.can create oval [expr $x - 5 - [lindex $coords 0]] [expr $y - 5 - [lindex $coords 1]] [expr $x + 5 - [lindex $coords 0]] [expr $y + 5 - [lindex $coords 1]] -outline [translate_color [lindex $params 4]] -width 3 -tags [list "[lindex $params 0]-eyeloc" eyeloc]
  }
}

add_agi_handler eyeloc emma_eyeloc


proc emma_cleareyeloc {cmd win coords params} {
  $win.can delete "[lindex $params 0]-eyeloc"
}

add_agi_handler cleareyeloc emma_cleareyeloc

