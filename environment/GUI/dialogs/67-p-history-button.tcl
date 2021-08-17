
set p_history_table [dict create]


frame [control_panel_name].p_history_display -borderwidth 0 

button [control_panel_name].p_history_display.show_trace -command p_history_trace -text "Production" -font button_font

tk_optionMenu [control_panel_name].p_history_display.which_trace current_p_history_trace ""

[control_panel_name].p_history_display.which_trace configure -font checkbox_font

[[control_panel_name].p_history_display.which_trace cget -menu] configure -font checkbox_font



pack [control_panel_name].p_history_display.which_trace -side right

pack [control_panel_name].p_history_display.show_trace -side left

pack [control_panel_name].p_history_display



proc p_history_trace {} {

  global current_p_history_trace
  global p_history_table


  if [dict exists $p_history_table $current_p_history_trace] {
    set cmd [dict get $p_history_table $current_p_history_trace]
    $cmd
  } else {
    tk_messageBox -title "No production history" -message "No production history type selected." -icon warning -type ok
  }
}

proc add_p_history_type {title cmd} {

  global current_p_history_trace
  global p_history_table

  set menu [[control_panel_name].p_history_display.which_trace cget -menu]
  set top [$menu entrycget 0 -value]

  $menu add radiobutton -label $title -variable current_p_history_trace

  if {$top == ""} {
    $menu delete ""
    set current_p_history_trace $title
  }

  dict set p_history_table $title $cmd
}
