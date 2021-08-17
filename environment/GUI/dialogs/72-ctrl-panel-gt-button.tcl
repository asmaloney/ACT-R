
set graphic_trace_table [dict create]


frame [control_panel_name].gt_display -borderwidth 0 

button [control_panel_name].gt_display.graphic_trace -command select_current_graphic_trace -text "Graphic Trace" -font button_font

tk_optionMenu [control_panel_name].gt_display.which_trace current_graphic_trace ""

[control_panel_name].gt_display.which_trace configure -font checkbox_font

[[control_panel_name].gt_display.which_trace cget -menu] configure -font checkbox_font



pack [control_panel_name].gt_display.which_trace -side right

pack [control_panel_name].gt_display.graphic_trace -side left

pack [control_panel_name].gt_display



proc select_current_graphic_trace {} {

  global current_graphic_trace
  global graphic_trace_table


  if [dict exists $graphic_trace_table $current_graphic_trace] {
    set cmd [dict get $graphic_trace_table $current_graphic_trace]
    $cmd
  } else {
    tk_messageBox -title "No graphic trace" -message "No graphic trace type selected." -icon warning -type ok
  }
}


proc add_graphic_trace_type {title cmd} {

  global current_graphic_trace
  global graphic_trace_table

  set menu [[control_panel_name].gt_display.which_trace cget -menu]
  set top [$menu entrycget 0 -value]

  $menu add radiobutton -label $title -variable current_graphic_trace

  if {$top == ""} {
    $menu delete ""
    set current_graphic_trace $title
  }

  dict set graphic_trace_table $title $cmd
}
