
proc make_text_trace_history_viewer {} {
  
  set win [toplevel [new_variable_name .text_trace_history]]
  
  wm withdraw $win

  wm geometry $win [get_configuration .text_trace_history $win]
  
  # frame and radio buttons for level

  set level_frame_1 [frame $win.level_frame_1 -borderwidth 0]  
  
  set rb1 [radiobutton $level_frame_1.rb1 -text "low" -variable $level_frame_1.level -value "low" -anchor w]
  set rb2 [radiobutton $level_frame_1.rb2 -text "medium" -variable $level_frame_1.level -value "medium" -anchor w]
  set rb3 [radiobutton $level_frame_1.rb3 -text "high" -variable $level_frame_1.level -value "high" -anchor w]
  set rb4 [radiobutton $level_frame_1.rb4 -text "ALL" -variable $level_frame_1.level -value "t" -anchor w]

  $rb1 invoke

  # set the range for the trace

  label $win.start -font text_font -text "start"
  label $win.end -font text_font -text "end"
 
  entry $win.min -text "" -font text_font
  entry $win.max -text "" -font text_font

  # The lables for the sections

  set l1 [label $win.l1 -text "Level" -justify left -font label_font]

  set l3 [label $win.l3 -text "" -justify left -font label_font -textvariable $win.l3.value]


  # frame for the trace display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -yscrollcommand "$text_frame_1.text_scrl set"  \
                                          -xscrollcommand "$text_frame_1.hscrl set" \
                                          -wrap none -font text_font -state disabled]
  
  # create the scroll bars for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 yview"]
  set h_scrl_bar [scrollbar $text_frame_1.hscrl -command "$text_box_1 xview" -orient horizontal]


 call_act_r_command "record-history" nil [list "trace-history"]

  bind $text_box_1 <Destroy> {
    call_act_r_command "stop-recording-history" nil [list "trace-history"]
  }
  


  button $win.get -text "Get History" -font button_font -command [list get_trace_history_data $text_box_1 $level_frame_1.level $win.min $win.max $win.l3.value]
  button $win.save -text "Save History" -font button_font -command [list save_trace_history_data $level_frame_1.level $win.min $win.max]
  button $win.load -text "Load History" -font button_font -command [list load_trace_history_data $text_box_1 $win.l3.value]




  pack $rb1 -side top -expand 1 -fill x
  pack $rb2 -side top -expand 1 -fill x
  pack $rb3 -side top -expand 1 -fill x
  pack $rb4 -side top -expand 1 -fill x

  pack $text_scroll_bar_1 -side right -fill y
  pack $h_scrl_bar -side bottom -fill x 
  pack $text_box_1 -side left -expand 1 -fill both


  place $l3 -relx 0.0 -y 0.0 -height 25 -relwidth 1.0
  place $text_frame_1 -relx .15 -y 25 -relheight 1.0 -height -25 -relwidth .85

  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .15
  place $level_frame_1 -relx 0.0 -y 50 -height 100 -relwidth .15

  place $win.start -relx 0.0 -y 150 -height 25 -relwidth .06
  place $win.min -relx .06 -y 150 -height 25 -relwidth .09
  place $win.end -relx 0.0 -y 175 -height 25 -relwidth .06
  place $win.max -relx .06 -y 175 -height 25 -relwidth .09

  place $win.get -relx 0 -y 200 -height 25 -relwidth .15
  place $win.save -relx 0 -y 225 -height 25 -relwidth .15
  place $win.load -relx 0 -y 250 -height 25 -relwidth .15


  # now show the window 

  wm deiconify $win
  focus $win

  return $win
}


proc get_trace_history_data {trace_window level_var min_entry max_entry top_label} {

  global $level_var
  global $top_label

  set min "nil"
  set max "nil"

  scan [$min_entry get] "%f" min
  scan [$max_entry get] "%f" max
  upvar $level_var level

  $trace_window configure -state normal
  $trace_window delete 1.0 end

  set any [call_act_r_command "history-data-available" nil [list "trace-history" $min $max $level]]

  if {$any == "nil" || $any == ""} {
    set $top_label "No Data available"
  } else {
    set $top_label "Collecting Data ..."

    set id [call_act_r_command "start-incremental-history-data" nil [list "trace-history" 16000 false $min $max $level]]

    if {$id != ""} {
      set result [get_incremental_history $id]

      foreach i $result {
     
        $trace_window insert end [lindex $i 3]
        $trace_window insert end "\n"
      }
   
      if {$level == "t"} {
        set detail "ALL"
      } else {
        set detail $level
      }

      set $top_label "Trace detail: $detail start: $min end: $max"
    } else {
      tk_messageBox -icon warning -type ok -title "Get Text History warning" \
                    -message "Unknown problem occurred trying to get data."
      set $top_label "Data Error occurred"
      
    }
  }
}


set trace_history_warnings ""

proc th_record_warnings {model s} {
  global trace_history_warnings

  set trace_history_warnings "$trace_history_warnings$s"
  return ""
}


proc save_trace_history_data {level_var min_entry max_entry} {

  global top_dir
  global trace_history_warnings

  set fname [tk_getSaveFile -title "Save Text History" -initialdir $top_dir] 

  if {$fname != ""} {

    global $level_var

    set min "nil"
    set max "nil"

    scan [$min_entry get] "%f" min
    scan [$max_entry get] "%f" max
    upvar $level_var level

    if {$level == "t"} {
      set detail "ALL"
    } else {
      set detail $level
    }

    set any [call_act_r_command "history-data-available" nil [list "trace-history" $min $max $level]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save Text History warning" \
                    -message "No data available to save with current settings."
    } else {
    
      set trace_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "th_record_warnings" "Environment command for capturing warnings during Save Text History."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" nil [list "trace-history" $fname "Data saved from Text Trace window. Trace detail: $detail start: $min end: $max" $min $max $level]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
      
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save Text Trace error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save Text Trace problem" \
                      -message "Save-history-data returned failure result.\n$trace_history_warnings."
      }
    }
  }
}


proc load_trace_history_data {trace_window top_label} {

  global top_dir
  global $top_label

  set fname [tk_getOpenFile -title "Load Text History" -initialdir $top_dir] 

  if {$fname != ""} {

    $trace_window configure -state normal
    $trace_window delete 1.0 end

    global trace_history_warnings

    set trace_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "th_record_warnings" "Environment command for capturing warnings during Load Text History."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    set id [call_act_r_command "start-incremental-history-data" nil [list "trace-history" 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
      
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load Text Trace problem" \
                    -message "Get-history-data returned failure result.\n$trace_history_warnings."
      set $top_label "Failure to load data"
    } else {

      set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]

      set result [get_incremental_history $id]

      foreach i $result {
        $trace_window insert end [lindex $i 3]
        $trace_window insert end "\n"
      }
   
      set $top_label [json::json2dict [lindex $comment 0]]
    }
  }
}


# add_history_button make_text_trace_history_viewer "Text Trace" :save-trace "Text trace" left get-saved-text-trace default-save-history-info

button [control_panel_name].text_trace -command {make_text_trace_history_viewer} \
       -text "Text Trace" -font button_font

# put that button on the control panel

pack [control_panel_name].text_trace
