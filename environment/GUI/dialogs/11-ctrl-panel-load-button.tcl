# The use_smart_load is going to be on the options panel, and it
# determines whether or not the file is just reloaded with the reload
# command (when it's nil) or whether the smart-loader function
# is used.  For now the default is t - use smart-loader
 

set load_result ""

proc record_load_traces {model s} {
  global load_result

  set load_result "$load_result$s"

  return ""
}

proc load_model_file {fname {wait ""}} {

  global load_result
  global options_array

  set load_result ""
            
  set load_monitor [add_new_cmd load_monitor "record_load_traces" "Environment command for capturing output during Load ACT-R code."]

  send_cmd "monitor" [list "model-trace" $load_monitor]
  send_cmd "monitor" [list "command-trace" $load_monitor]
  send_cmd "monitor" [list "warning-trace" $load_monitor]
  send_cmd "monitor" [list "general-trace" $load_monitor]

  set result [call_act_r_command_with_error_messages "load-act-r-code" nil [list {$fname} $options_array(use_smart_load)]]

  send_cmd "remove-monitor" [list "model-trace" $load_monitor]
  send_cmd "remove-monitor" [list "command-trace" $load_monitor]
  send_cmd "remove-monitor" [list "warning-trace" $load_monitor]
  send_cmd "remove-monitor" [list "general-trace" $load_monitor]
     
  remove_cmd $load_monitor

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

  set the_button [button $win.but -text "Ok" -font button_font -command "destroy $win"]

  place $text_frame -x 0 -y 0 -relheight 1.0 -height -30 -relwidth 1.0
  place $the_button -relx .5 -x -30 -width 60 -rely 1.0 -y -30 -height 30

  pack $text_scroll_bar -side right -fill y
  pack $text_box -side left -expand 1 -fill both

  if {[lindex $result 0] == 0} {
    wm title $win "ERROR Loading"
    $text_box insert 1.0 "$load_result\n[lindex $result 1]"
  } else {
    wm title $win "SUCCESSFUL Load"
    $text_box insert 1.0 $load_result
  }

  wm deiconify $win
  focus $win

  if {$wait != ""} {tkwait window $win}
}


button [control_panel_name].load -text "Load ACT-R code" -font button_font -command {
  global local_connection
  global top_dir
  global current_file_window
  global currently_open_files

  set fname ""

  if {$local_connection == 0} {
    tk_messageBox -icon warning -type ok -title "Load warning" \
                  -message "You cannot use the Load ACT-R code button if the\
                            environment is not running on the same machine\
                            as ACT-R."  
  } else {
    if {$current_file_window == ""} {
      set fname [tk_getOpenFile -title "File to load" -initialdir $top_dir]
    } else {
      set fname [tk_getOpenFile -title "File to load" -initialdir [file dirname $currently_open_files($current_file_window)] -initialfile [file tail $currently_open_files($current_file_window)]]
    }

    if {$fname != ""} {
      load_model_file $fname
    }
  }              
}

pack [control_panel_name].load
