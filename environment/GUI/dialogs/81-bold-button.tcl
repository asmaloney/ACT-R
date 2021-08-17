
set bold_view_table [dict create]


frame [control_panel_name].bold_display -borderwidth 0 

button [control_panel_name].bold_display.show_trace -command display_bold_trace -text "BOLD" -font button_font

tk_optionMenu [control_panel_name].bold_display.which_trace current_bold_trace ""

[control_panel_name].bold_display.which_trace configure -font checkbox_font

[[control_panel_name].bold_display.which_trace cget -menu] configure -font checkbox_font



pack [control_panel_name].bold_display.which_trace -side right

pack [control_panel_name].bold_display.show_trace -side left

pack [control_panel_name].bold_display



proc display_bold_trace {} {

  global current_bold_trace
  global bold_view_table


  if [dict exists $bold_view_table $current_bold_trace] {
    set cmd [dict get $bold_view_table $current_bold_trace]
    $cmd
  } else {
    tk_messageBox -title "No BOLD trace" -message "No BOLD trace type selected." -icon warning -type ok
  }
}


proc add_bold_trace_type {title cmd} {

  global current_bold_trace
  global bold_view_table

  set menu [[control_panel_name].bold_display.which_trace cget -menu]
  set top [$menu entrycget 0 -value]

  $menu add radiobutton -label $title -variable current_bold_trace

  if {$top == ""} {
    $menu delete ""
    set current_bold_trace $title
  }

  dict set bold_view_table $title $cmd
}
