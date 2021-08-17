proc stop_model {} {

  call_act_r_command "schedule-break-relative" "nil" [list 0]

  # my json parser along with call_act_r_command can't create the options list... [list [list "details" "Environment Stop Button pressed"]]]
}

button [control_panel_name].stop_button \
       -command {stop_model} -text "Stop" -font button_font


pack [control_panel_name].stop_button

