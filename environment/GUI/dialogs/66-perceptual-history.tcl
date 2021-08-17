
proc make_percept_history_viewer {name window cmd} {
  
  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "$name History" -message "$name history viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name $window]]
  
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration $window $win]
  
    # frame and list box for times

    set list_frame_1 [frame $win.list_frame_1 -borderwidth 0]  
  
    set list_box_1 [listbox $list_frame_1.list_box -listvar \
                            $list_frame_1.list_box.var \
                            -yscrollcommand "$list_frame_1.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

  
    set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl -command "$list_box_1 yview"]

    # The lables for the sections

    set l1 [label $win.l1 -text "Times" -justify left -font label_font]
    set l3 [label $win.l3 -text "" -justify left -font label_font -textvariable $win.l3.value]

    # frame for the display

    set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
    set text_box_1 [text $text_frame_1.text -yscrollcommand "$text_frame_1.text_scrl set"  \
                                            -xscrollcommand "$text_frame_1.hscrl set" \
                                            -wrap none -font text_font]
 
    # create the scroll bars for the text box
  
    set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 yview"]
    set h_scrl_bar [scrollbar $text_frame_1.hscrl -command "$text_box_1 xview" -orient horizontal]

    # hold the history info when it's gotten

    global $win.data
    set $win.data ""

    # make the selection list call the display updater for the audicon

    bind $list_box_1 <<ListboxSelect>> "select_percept_history $list_box_1 $text_box_1 $win.data"

    button $win.get -text "Get History" -font button_font -command "get_percept_history_data $cmd $name $list_box_1 $text_box_1 $win.l3.value $model $win.data"
    button $win.save -text "Save History" -font button_font -command "save_percept_history_data $cmd $name $model"
    button $win.load -text "Load History" -font button_font -command "load_percept_history_data $cmd $name $list_box_1 $text_box_1 $win.l3.value $model $win.data"

    
    pack $list_scroll_bar_1 -side right -fill y 
    pack $list_box_1 -side left -expand 1 -fill both
    pack $text_scroll_bar_1 -side right -fill y
    pack $h_scrl_bar -side bottom -fill x 
    pack $text_box_1 -side left -expand 1 -fill both

    place $l1 -relx 0.0 -y 0 -height 25 -relwidth .10
    place $list_frame_1 -relx 0.0 -y 25 -relheight 1.0 -height -50 -relwidth .10

    place $l3 -relx .1 -y 0 -height 25 -relwidth .9
    place $text_frame_1 -relx .1 -y 25 -relheight 1.0 -height -50 -relwidth .9
  
    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

  
    # record the appropriate history info

    call_act_r_command record-history $model [list $cmd]

    bind $text_box_1 <Destroy> "call_act_r_command stop-recording-history $model [list $cmd]
                                global $win.data
                                unset $win.data"

    # now show the window 

    wm deiconify $win
    focus $win

    return $win
  }
}

proc get_percept_history_data {cmd name timelst txt label model data_var} {

  global $label

  $timelst delete 0 end
  $txt delete 1.0 end

  set any [call_act_r_command "history-data-available" $model [list $cmd]]

  if {$any == ""} {
    set $label "No Data available"
  } else {
    set $label "Collecting Data ..."

    set id [call_act_r_command "start-incremental-history-data" $model [list $cmd 16000 false]]
 
    if {$id != ""} {
      set result [get_incremental_history $id]
    } else {
      tk_messageBox -icon warning -type ok -title "Get $name History warning" \
                    -message "Unknown problem occurred trying to get data."
      set $label "Data Error occurred"
      return "" 
    }

    global $data_var
    unset $data_var 

    upvar $data_var data


    foreach i $result {
      set time [expr [lindex $i 0] / 1000.0 ]

      $timelst insert end $time
      set txt ""

      foreach j [lrange $i 1 end] {
        append txt $j
      }

      set data($time) $txt
    }
      
    set $label $name
  }
}

proc percept_record_warnings {model s} {
  global percept_history_warnings

  set percept_history_warnings "$percept_history_warnings$s"
  return ""
}

proc load_percept_history_data {cmd name timelst txt label model data_var} {

  global top_dir
  global $label

  set fname [tk_getOpenFile -title "Load $name History" -initialdir $top_dir] 

  if {$fname != ""} {

    set $label ""

    $timelst delete 0 end
    $txt delete 1.0 end

    global percept_history_warnings

    set percept_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "percept_record_warnings" "Environment command for capturing warnings during Load $name History."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    set id [call_act_r_command "start-incremental-history-data" $model [list $cmd 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"}  {
      tk_messageBox -icon warning -type ok -title "Load $name History problem" \
                    -message "Get-history-data returned failure result.\n$percept_history_warnings."
    } else {

      set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]

      set result [get_incremental_history $id]

      global $data_var
      unset $data_var 

      upvar $data_var data

      foreach i $result {
        set time [expr [lindex $i 0] / 1000.0 ]

        $timelst insert end $time
        set txt ""

        foreach j [lrange $i 1 end] {
          append txt $j
        }

        set data($time) $txt
      }
      
      set $label [json::json2dict [lindex $comment 0]]
    }
  }
}



proc save_percept_history_data {cmd name model} {

  global top_dir
  global percept_history_warnings

  set fname [tk_getSaveFile -title "Save $name History" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list $cmd]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save $name History warning" \
                    -message "No data available to save with current settings."
    } else {
    
      set percept_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "percept_record_warnings" "Environment command for capturing warnings during Save $name History."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list $cmd $fname "Data saved from $name History window for model $model."]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
      
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save $name History error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save $name History problem" \
                      -message "Save-history-data returned failure result.\n$percept_history_warnings."
      }
    }
  }
}

proc select_percept_history {timelst text data_var} {
    
  $text delete 1.0 end

  upvar $data_var data
  
  set selections [$timelst curselection]

  if {[llength $selections] != 0} {
    set time [$timelst get [lindex $selections 0]]
  
    $text insert 1.0 $data($time)
 
  }
}


button [control_panel_name].audicon_history -command {make_percept_history_viewer "Audicon" ".audicon_history" "audicon-history"} -text "Audicon History" -font button_font
button [control_panel_name].visicon_history -command {make_percept_history_viewer "Visicon" ".visicon_history" "visicon-history"} -text "Visicon History" -font button_font

# put that button on the control panel

pack [control_panel_name].audicon_history

pack [control_panel_name].visicon_history
