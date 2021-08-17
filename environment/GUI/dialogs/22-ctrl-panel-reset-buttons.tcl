global reload_return
global during_reload

set during_reload 0

frame [control_panel_name].r_frame -borderwidth 0

set reset_warnings ""

proc record_reset_warnings {model s} {
  global reset_warnings

  set reset_warnings "$reset_warnings$s"
  return ""
}

button [control_panel_name].r_frame.reset -text "Reset" -font button_font -command {

  global reset_warnings

  [control_panel_name].r_frame.reset configure -state disabled

  set reset_warnings ""

  set reset_monitor [add_new_cmd reset_monitor "record_reset_warnings" "Environment command for capturing warnings during a call to reset."]

  send_cmd "monitor" [list "warning-trace" $reset_monitor]

  set result [call_act_r_command_with_error_messages "reset"]

  send_cmd "remove-monitor" [list "warning-trace" $reset_monitor]
     
  remove_cmd $reset_monitor

 if {[lindex $result 0] == 0} {
    tk_messageBox -icon warning -type ok -title "Reset Error" \
                  -message [lindex $result 1]
  } elseif {[lindex $result 1] == "null"} {
    tk_messageBox -icon warning -type ok -title "Reset Error" \
                  -message $reset_warnings
  } else {
    call_act_r_command "act-r-output" "nil" [list [format "#|## ACT-R has been Reset ##|#"]]
  }

  set reset_warnings ""

  [control_panel_name].r_frame.reset configure -state normal
} 


button [control_panel_name].r_frame.reload -text "Reload" -font button_font -command {
  global options_array
  global during_reload
  global reset_warnings


  set during_reload 1

  [control_panel_name].r_frame.reload configure -state disabled

  set reset_warnings ""

  global currently_open_files

  if {$options_array(save_before_reload) == 1 && [array exists currently_open_files]} {
    foreach win [array names currently_open_files] {
      save_model $win
    }
  }
  

  set reload_monitor [add_new_cmd reload_monitor "record_reset_warnings" "Environment command for capturing output during a call to reload."]


  send_cmd "monitor" [list "model-trace" $reload_monitor]
  send_cmd "monitor" [list "command-trace" $reload_monitor]
  send_cmd "monitor" [list "warning-trace" $reload_monitor]
  send_cmd "monitor" [list "general-trace" $reload_monitor]

  if {$options_array(use_smart_load) == "t"} {
    set result [call_act_r_command_with_error_messages "reload" nil [list $options_array(use_smart_load)]]
  } else {
    set result [call_act_r_command_with_error_messages "reload"]
  }

  send_cmd "remove-monitor" [list "model-trace" $reload_monitor]
  send_cmd "remove-monitor" [list "command-trace" $reload_monitor]
  send_cmd "remove-monitor" [list "warning-trace" $reload_monitor]
  send_cmd "remove-monitor" [list "general-trace" $reload_monitor]
     
  remove_cmd $reload_monitor

  set win [toplevel [new_variable_name .reload_response]]
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  wm geometry $win [get_configuration .reload_response $win]


  set text_frame [frame $win.text_frame -borderwidth 0]  
 
  set text_box [text $text_frame.text -yscrollcommand \
                     "$text_frame.text_scrl set" -state normal \
                     -font text_font]
  
  
  
  set text_scroll_bar [scrollbar $text_frame.text_scrl \
                                 -command "$text_box yview"]


  set the_button [button $win.but -text "Ok" -command "destroy $win" -font button_font]

  place $text_frame -x 0 -y 0 -relheight 1.0 -height -30 -relwidth 1.0
  place $the_button -relx .5 -x -30 -width 60 -rely 1.0 -y -30 -height 30

  pack $text_scroll_bar -side right -fill y
  pack $text_box -side left -expand 1 -fill both


 if {[lindex $result 0] == 0} {
    wm title $win "ERROR Reloading"
    $text_box insert 1.0 "$reset_warnings\n[lindex $result 1]"
  } elseif {[lindex $result 1] == "null"} {
    wm title $win "ERROR Reloading"
    $text_box insert 1.0 "$reset_warnings"
  } else {
    wm title $win "SUCCESSFUL Reload"
    $text_box insert 1.0 "$reset_warnings"
    call_act_r_command "act-r-output" "nil" [list [format "#|## Reload Complete ##|#"]]
  }

  set during_reload 0

  [control_panel_name].r_frame.reload configure -state normal

  wm deiconify $win
  focus $win
}
      
pack [control_panel_name].r_frame.reset -side left
pack [control_panel_name].r_frame.reload -side right

pack [control_panel_name].r_frame

