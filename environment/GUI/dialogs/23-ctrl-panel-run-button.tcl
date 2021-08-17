frame [control_panel_name].run_frame -borderwidth 0 -width 130

entry [control_panel_name].run_frame.run_time -width 6 -font text_font \
      -textvariable run_button_time -justify right

[control_panel_name].run_frame.run_time insert 0 "10.0"

set run_warnings ""


proc record_run_warnings {model s} {
  global run_warnings

  set run_warnings "$run_warnings$s"
  return ""
}

button [control_panel_name].run_frame.run -text "Run" -font button_font -command {

  global run_warnings

  [control_panel_name].run_frame.run configure -state disabled

  set run_warnings ""

  set run_monitor [add_new_cmd run_monitor "record_run_warnings" "Environment command for capturing warnings during a call to run."]

  send_cmd "monitor" [list "warning-trace" $run_monitor]

  set result [call_act_r_command_with_error_messages "run" "nil" [list $run_button_time]]

  send_cmd "remove-monitor" [list "warning-trace" $run_monitor]
     
  remove_cmd $run_monitor

  if {[lindex $result 0] == 0} {
    tk_messageBox -icon warning -type ok -title "Run Error" \
                  -message [lindex $result 1]
  } elseif {[lindex $result 1] == "null"} {
    tk_messageBox -icon warning -type ok -title "Run Error" \
                  -message $run_warnings
  }

  set $run_warnings ""

  [control_panel_name].run_frame.run configure -state normal
}                    

pack [control_panel_name].run_frame.run -side left
pack [control_panel_name].run_frame.run_time -side right

pack [control_panel_name].run_frame

