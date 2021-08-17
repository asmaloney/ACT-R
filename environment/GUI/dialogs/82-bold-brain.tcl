image create photo brain -file [file join $tcl_env_dir dialogs ref-brain.gif]

proc make_bold_brains {} {
  
  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "BOLD 2D brain" -message "BOLD viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .bold_brain]]
  
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .bold_slices $win]
  
    set frame [frame $win.frame -borderwidth 0]  
 
    canvas $win.frame.canvas  -bg gray\
           -xscrollcommand "$win.frame.scrlx set" \
           -yscrollcommand "$win.frame.scrly set" \
           -width 740 -height 640 -scrollregion {0 0 740 640}
          
    scrollbar $win.frame.scrlx -command "$win.frame.canvas xview" -orient horizontal
    scrollbar $win.frame.scrly -command "$win.frame.canvas yview" -orient vertical


    $win.frame.canvas create image 100 0 -image brain -anchor nw

    initialize_brain_scans $win

    global $win.data
    set $win.data ""
   
    call_act_r_command record-history $model [list "bold-prediction-with-time-scaled"]

    bind $win.frame.canvas <Destroy> "
      call_act_r_command stop-recording-history $model [list "bold-prediction-with-time-scaled"]
      global $win.data
      unset $win.data
    "

    scale $win.s -orient horizontal -command "report_scale_brain $win" -from 0 -to 0 -label "Scan #"  -font checkbox_font -tickinterval 0

    button $win.get -text "Get History" -font button_font -command "get_brain_history_data $win $model $win.frame.canvas $win.s"
    button $win.save -text "Save History" -font button_font -command "save_brain_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_brain_history_data $win $model $win.frame.canvas $win.s"

    label $win.details -font label_font 

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    place $win.frame.canvas -relx 0 -rely 0 -relwidth 1.0 -relheight 1.0

    place $win.frame -x 0 -y 0 -height -125 -relheight 1.0 -relwidth 1.0

    place $win.s -relwidth 1.0 -rely 1.0 -y -125 -height 75

    place $win.details -x 0 -rely 1.0 -y -50 -height 25 -relwidth 1.0

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

    wm deiconify $win

    pick_buffers
  }
}


proc initialize_brain_scans {win} {

  $win.frame.canvas delete box
  $win.frame.canvas delete text

    $win.frame.canvas create rectangle 262 68 272 78  -width 0 -fill "" -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 390 68 400 78  -width 0 -fill "" -outline "" -tag {box manual} 
    $win.frame.canvas create rectangle 518 68 528 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 646 68 656 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 316 68 326 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 444 68 454 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 572 68 582 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 700 68 710 78  -width 0 -fill ""  -outline "" -tag {box manual}
    $win.frame.canvas create rectangle 390 602 400 612  -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 518 602 528 612  -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 646 602 656 612 -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 444 602 454 612 -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 572 602 582 612 -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 700 602 710 612 -width 0 -fill ""  -outline "" -tag {box visual}
    $win.frame.canvas create rectangle 542 46 548 56 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 670 46 676 56 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 158 174 164 184 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 286 174 292 184 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 550 46 556 56 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 678 46 684 56 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 166 174 172 184 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 294 174 300 184 -width 0 -fill ""  -outline "" -tag {box goal}
    $win.frame.canvas create rectangle 260 190 270 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 388 190 398 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 516 190 526 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 644 190 654 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 316 190 326 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 444 190 454 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 572 190 582 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 700 190 710 200 -width 0 -fill ""  -outline "" -tag {box vocal}
    $win.frame.canvas create rectangle 274 224 284 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 402 224 412 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 530 224 540 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 658 224 668 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 304 224 314 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 432 224 442 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 560 224 570 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 688 224 698 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
    $win.frame.canvas create rectangle 388 164 398 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 516 164 526 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 644 164 654 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 132 292 142 302 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 444 164 454 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 572 164 582 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 700 164 710 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 188 292 198 302 -width 0 -fill ""  -outline "" -tag {box retrieval}
    $win.frame.canvas create rectangle 536 300 544 308 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 664 300 672 308 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 152 428 160 436 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 280 428 288 436 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 554 300 562 308 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 682 300 690 308 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 170 428 178 436 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 298 428 306 436 -width 0 -fill ""  -outline "" -tag {box procedural}
    $win.frame.canvas create rectangle 642 322 652 332 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 130 450 140 460 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 258 450 268 460 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 386 450 396 460 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 702 322 712 332 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 190 450 200 460 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 318 450 328 460 -width 0 -fill ""  -outline "" -tag {box aural}
    $win.frame.canvas create rectangle 446 450 456 460 -width 0 -fill ""  -outline "" -tag {box aural}

    $win.frame.canvas create text  10  120 -text "manual" -fill gray -anchor w -font checkbox_font -tag {text manual.text}
    $win.frame.canvas create text  10  140 -text "goal" -fill gray -anchor w  -font checkbox_font -tag {text goal.text}
    $win.frame.canvas create text  10  160 -text "vocal" -fill gray -anchor w -font checkbox_font  -tag {text vocal.text}
    $win.frame.canvas create text  10  180 -text "imaginal" -fill gray -anchor w -font checkbox_font -tag {text imaginal.text}
    $win.frame.canvas create text  10  200 -text "retrieval" -fill gray -anchor w -font checkbox_font -tag {text retrieval.text}
    $win.frame.canvas create text  10  400 -text "procedural" -fill gray -anchor w -font checkbox_font -tag {text procedural.text}
    $win.frame.canvas create text  10  420 -text "aural" -fill gray -anchor w -font checkbox_font -tag {text aural.text}
    $win.frame.canvas create text  10  440 -text "visual" -fill gray -anchor w -font checkbox_font -tag {text visual.text}
}



proc make_brain_color {name level} {
 
  set rs 0
  set gs 0
  set bs 0

  switch $name {
    manual {
      set rs 1
    }
    goal {
      set gs 1
    }
    vocal {
      set bs 1
    }
    imaginal {
      set rs 1
      set gs 1
    }
    retrieval {
      set rs 1
      set bs 1
    }
    production {
      set rs 1
      set bs 1
    }
    aural {
      set gs 1
    }
    visual {
      set bs 1
    }
  }
  return [format "#%.2x%.2x%.2x" [expr round($rs * $level * 255)]  [expr round($gs * $level * 255)] [expr round($bs * $level * 255)] ]
}



proc report_scale_brain {win s} {

  upvar #0 $win.data data

  if {$s == 0} {
    $win.frame.canvas itemconfigure box -fill ""

    foreach x $data {
      $win.frame.canvas itemconfigure [lindex $x 0] -width 1 -outline [make_brain_color [lindex $x 0] .75]
      $win.frame.canvas itemconfigure [lindex $x 0].text -fill [make_brain_color [lindex $x 0] .75]
    }
  } elseif {$s > 0} {
    foreach x $data {
      $win.frame.canvas itemconfigure [lindex $x 0] -fill [make_brain_color [lindex $x 0] [lindex $x $s]]
    }
  }
}


set brain_graph_history_warnings ""

proc brain_graph_record_warnings {model s} {
  global brain_graph_history_warnings

  set brain_graph_history_warnings "$brain_graph_history_warnings$s"
  return ""
}


proc save_brain_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save BOLD Data" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "bold-prediction-with-time-scaled"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save BOLD warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global brain_graph_history_warnings

      set brain_graph_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "brain_graph_record_warnings" "Environment command for capturing warnings during Save BOLD brain trace."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "bold-prediction-with-time-scaled" $fname "Data saved from BOLD 2D brain window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save BOLD brain error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save BOLD brain problem" \
                      -message "Save-history-data returned failure result.\n$brain_graph_history_warnings."
      }
    }
  }
}

proc load_brain_history_data {win model canvas scale} {

  global top_dir

  set fname [tk_getOpenFile -title "Load BOLD History" -initialdir $top_dir] 

  if {$fname != ""} {
    get_brain_history_data $win $model $canvas $scale $fname
  }
}

proc get_brain_history_data {win model canvas scale {fname ""}} {

  $win.get configure -state disabled
  $win.save configure -state disabled
  $win.load configure -state disabled

  initialize_brain_scans $win

  $win.details configure -text ""

  if {$fname == ""} {
    set any [call_act_r_command "history-data-available" $model [list "bold-prediction-with-time-scaled"]]

    if {$any == "nil" || $any == ""} {
      $win.details configure -text "No Data available"
      set data [list "nothing"]
    } else {
      $win.details configure -text "Collecting Data ..."
      set id [call_act_r_command "start-incremental-history-data" $model [list "bold-prediction-with-time-scaled" 16000]]
    
      if {$id != ""} {
        set data [get_incremental_history $id]
      } else {
        tk_messageBox -icon warning -type ok -title "Get BOLD History warning" \
                      -message "Unknown problem occurred trying to get data."
        $win.details configure -text "Data Error occurred"

        $win.get configure -state normal
        $win.save configure -state normal
        $win.load configure -state normal

        return "" 
      }

      $win.details configure -text "BOLD data for model $model"
    }
  } else {
    global brain_graph_history_warnings
    set brain_graph_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "brain_graph_record_warnings" "Environment command for capturing warnings during Load BOLD brain data."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    $win.details configure -text "Loading Data from file $fname"

    set id [call_act_r_command "start-incremental-history-data" $model [list "bold-prediction-with-time-scaled" 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load BOLD problem" \
                    -message "Get-history-data returned failure result.\n$brain_graph_history_warnings."
  
      $win.details configure -text "Failure to load data"
      $win.get configure -state normal
      $win.save configure -state normal
      $win.load configure -state normal

      return ""
    } else {
      set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]

      set comment "[json::json2dict [lindex $comment 0]] loaded from file $fname"
      set data [get_incremental_history $id]
    }
  }

  if {$data != "nothing"} {

    $scale configure -to [expr [llength [lindex $data 0]] - 1 ] -tickinterval [expr [llength [lindex $data 0]] - 1 ]
    upvar #0 $win.data d
    set d ""

    foreach x [lrange $data 1 end] {
      lappend d [lreplace $x 0 0 [string tolower [lindex $x 0]]]
    }

    report_scale_brain $win 0
    $scale set 0

  } else {
    # should have more error checking...  
  }
  
  $win.get configure -state normal
  $win.save configure -state normal
  $win.load configure -state normal


  if {$fname == "" && $data != "nothing"} {
    $win.details configure -text "BOLD data for model $model"
  } elseif {$fname != ""} {
    $win.details configure -text $comment
  }
}

add_bold_trace_type "2D brain" make_bold_brains
