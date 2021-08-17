frame [control_panel_name].run_display -borderwidth 0 

label [control_panel_name].run_display.running_indicator -text "-------" -font label_font -foreground black -width 7


checkbutton [control_panel_name].run_display.enable_running -text "Show Run State" -font checkbox_font \
                -variable check_running -command {change_running_indicator} 

pack [control_panel_name].run_display.running_indicator -side left

pack [control_panel_name].run_display.enable_running -side right

pack [control_panel_name].run_display


set check_running 0

set run_indicator "[control_panel_name].run_display.running_indicator"

proc indicate_running {model time} {
  global run_indicator

  $run_indicator configure -text "Running" -foreground darkgreen
}

proc indicate_not_running {model time} {
  global run_indicator

  $run_indicator configure -text "Stopped" -foreground red
}

set x [add_new_cmd running_indicator "indicate_running" "Environment command for detecting that ACT-R is running to display on control panel."]

set runnig_indicator_start_fct $x


set x [add_new_cmd run_stop_indicator "indicate_not_running" "Environment command for detecting that ACT-R has stopped running to display on control panel."]


set runnig_indicator_stop_fct $x


proc enable_running_indicator {} {
  global runnig_indicator_stop_fct
  global runnig_indicator_start_fct

  send_cmd "monitor" [list "run-stop" $runnig_indicator_stop_fct]
  send_cmd "monitor" [list "run-start" $runnig_indicator_start_fct]
}


proc disable_running_indicator {} {
  global runnig_indicator_stop_fct
  global runnig_indicator_start_fct
  global run_indicator

  $run_indicator configure -text "-------" -foreground black
  send_cmd "remove-monitor" [list "run-stop" $runnig_indicator_stop_fct]
  send_cmd "remove-monitor" [list "run-start" $runnig_indicator_start_fct]
}

proc change_running_indicator {} {
  global check_running

  if $check_running {
    enable_running_indicator
  } else {
    disable_running_indicator
  }
}

    