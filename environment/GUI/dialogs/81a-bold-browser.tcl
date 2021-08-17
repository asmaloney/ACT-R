
set bold_color_list [list #ff0000 #8080ff #008000 #ff8000 #7f7f7f #ff00ff #ffff00 #c08000 #f0f0f0 #c000c0 #00c050 #f0c0ff #c05850 #50ff25]

proc make_bold_multi_graphs {} {
  
 set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "BOLD graph" -message "BOLD graph viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .bold_graphs]]
  
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .bold_graphs $win]

    set list_frame [frame $win.list_frame -borderwidth 0]  

    set list_box [listbox $list_frame.list_box -listvar \
                          $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode multiple \
                          -exportselection 0 -font list_font -bd 0]

    set list_scroll_bar [scrollbar $list_frame.list_scrl -command "$list_box yview"]


    set frame [frame $win.frame -borderwidth 0]  
 
    canvas $win.frame.canvas  \
           -xscrollcommand "$win.frame.scrlx set" \
           -yscrollcommand "$win.frame.scrly set" \
           -bg white
          
    scrollbar $win.frame.scrlx -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly -command "$win.frame.canvas yview" -orient vertical

    bind $list_box <<ListboxSelect>> "select_multi_buffer_graph $list_box $win.frame.canvas $win"

    label $win.details -font label_font 

    set button_frame [frame $win.buttons -borderwidth 0]


    radiobutton $win.buttons.grid0 -text "Raw values" -font button_font -value "raw" -variable $win.buttons.value
    radiobutton $win.buttons.grid1  -command "select_multi_buffer_graph $list_box $win.frame.canvas $win" -text "Scale each" -font button_font -value "each" -variable $win.buttons.value
    radiobutton $win.buttons.grid2 -command "select_multi_buffer_graph $list_box $win.frame.canvas $win" -text "Scale all" -font button_font -value "all" -variable $win.buttons.value

    $win.buttons.grid0 invoke
    $win.buttons.grid0 configure -command "select_multi_buffer_graph $list_box $win.frame.canvas $win"

    button $win.buttons.zoom_in -command "bold_multi_zoom_in $win" -text "+" -font button_font
    button $win.buttons.zoom_out -command  "bold_multi_zoom_out $win" -text "-" -font button_font


    button $win.get -text "Get History" -font button_font -command "get_bold_graph_history_data $win $model $list_box $win.frame.canvas"
    button $win.save -text "Save History" -font button_font -command "save_bold_graph_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_bold_graph_history_data $win $model $list_box $win.frame.canvas"


    call_act_r_command record-history $model [list "bold-prediction-with-time"]

    global $win.scale

    set $win.scale 1.0

    global $win.data

    set $win.data ""

    bind $win.frame.canvas <Destroy> "
      call_act_r_command stop-recording-history $model [list "bold-prediction-with-time"]
      global $win.scale
      unset $win.scale
      global $win.data
      unset $win.data
    "

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    pack $win.frame.canvas -side left -fill both

    pack $list_scroll_bar -side right -fill y 
    pack $list_box -side left -expand 1 -fill both

    place $win.buttons.grid0 -x 0 -y 0 -width 95 -height 24
    place $win.buttons.grid1 -x 100 -y 0 -width 95 -height 24
    place $win.buttons.grid2 -x 200 -y 0 -width 95 -height 24

    place $win.buttons.zoom_in -relx 1.0 -x -100 -y 0 -width 45 -height 24
    place $win.buttons.zoom_out -relx 1.0 -x -50 -y 0 -width 45 -height 24

    place $win.buttons -x 0 -y 0 -height 25 -relwidth 1.0
    place $win.details -x 0 -y 25 -relwidth 1.0 -height 25

    place $list_frame -relx 0.0 -y 50 -relheight 1.0 -height -75 -relwidth .2
    place $win.frame -relx .25 -y 50 -relheight 1.0 -height -75 -relwidth .75

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

    wm deiconify $win

    pick_buffers
  }
}

proc bold_multi_zoom_out {win} {

  upvar #0 $win.scale scale

  if {[$win.frame.canvas cget -scrollregion] != ""} {
    set scale [expr .5 * $scale]
    resize_window $win .5
  }
}

proc resize_window {win scale} {

  $win.frame.canvas scale all 0 0 $scale 1.0
  $win.frame.canvas configure -scrollregion "0 0 [expr $scale * [lindex [$win.frame.canvas cget -scrollregion] 2]] [$win.frame.canvas cget -height]"
  $win.frame.canvas configure -width [expr $scale * [$win.frame.canvas cget -width]]
}

proc bold_multi_zoom_in {win} {

  upvar #0 $win.scale scale

  if {[$win.frame.canvas cget -scrollregion] != ""} {
    set scale [expr 2 * $scale]
    resize_window $win 2
  }    
}


set bold_pps 100
set bold_v_scale 300

proc select_multi_buffer_graph {listwin target_win win} {

  global bold_pps
  global bold_v_scale

  $target_win delete all

  set selections [$listwin curselection]

  if {[llength $selections] != 0} {

    upvar #0 $win.scale scale
    upvar #0 $win.data data
    upvar #0 $win.buttons.value value

    set scale 1.0

    set times $data(times)

    if {[llength $times] > 1} {
  
      set w [expr round(40 + ($bold_pps * ([lindex $times 0] + [lindex $times end])))]
      set h [expr round(20 + (13 * $bold_v_scale / 10))]

      $target_win configure -height $h -width $w -scrollregion "0 0 $w $h"
    
      $target_win create line 40 0 40 [expr $h - 20] -fill black -width 1
      

      for {set y 0} {$y <= [expr $h - 20]} {incr y [expr round($bold_v_scale / 10)]} {
        $target_win create line 36 $y 44 $y -fill black -width 1
      }

      set zero [expr round(11 * $bold_v_scale / 10)]

      $target_win create line 40 $zero $w $zero -fill black -width 1

      $target_win create text 5 $zero -text "0.0" -fill black -anchor w
      $target_win create text 5 [expr round($bold_v_scale / 10)] -text "1.0" -fill black -anchor w

      foreach t $times {
        $target_win create line [expr round(40 + ($bold_pps * $t))] [expr $zero - 3] [expr round(40 + ($bold_pps * $t))] [expr $zero + 3] -fill black -width 1
        $target_win create text [expr round(40 + ($bold_pps * $t))] [expr $h - 20] -font env_window_font -fill black -text [format "%.3f" $t] -anchor n
      }

      foreach index $selections {

        set buf [$listwin get $index]

        set d $data($buf)
        set c $data($buf.color)

        switch $value {
          raw {set s 1.0}
          all {set s $data(max)}
          each {set s $data($buf.max)}
        }

        set prev [lindex $d 0]
        set i 0
  
        foreach y [lrange $d 1 end] {

          $target_win create line [expr round(40 + ($bold_pps * [lindex $times $i]))] [expr round($zero - ($bold_v_scale * $prev / $s))] \
                                  [expr round(40 + ($bold_pps * [lindex $times [expr $i + 1]]))] [expr round($zero - ($bold_v_scale * $y / $s))] \
                                  -fill $c -width 2
          set prev $y
          incr i
        }
      } 
    }
  }
}


set bold_graph_history_warnings ""

proc bold_graph_record_warnings {model s} {
  global bold_graph_history_warnings

  set bold_graph_history_warnings "$bold_graph_history_warnings$s"
  return ""
}


proc save_bold_graph_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save BOLD Data" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "bold-prediction-with-time"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save BOLD warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global bold_graph_history_warnings

      set bold_graph_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "bold_graph_record_warnings" "Environment command for capturing warnings during Save BOLD graph trace."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "bold-prediction-with-time" $fname "Data saved from BOLD graph window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save BOLD graph error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save BOLD graph problem" \
                      -message "Save-history-data returned failure result.\n$bold_graph_history_warnings."
      }
    }
  }
}

proc load_bold_graph_history_data {win model list_box canvas} {

  global top_dir

  set fname [tk_getOpenFile -title "Load BOLD History" -initialdir $top_dir] 

  if {$fname != ""} {
    get_bold_graph_history_data $win $model $list_box $canvas $fname
  }
}

proc get_bold_graph_history_data {win model list_box canvas {fname ""}} {

  $win.buttons.zoom_in configure -state disabled
  $win.buttons.zoom_out configure -state disabled
  $win.buttons.grid0 configure -state disabled
  $win.buttons.grid1 configure -state disabled
  $win.buttons.grid2 configure -state disabled
  $win.get configure -state disabled
  $win.save configure -state disabled
  $win.load configure -state disabled

  $canvas delete all 
  $list_box delete 0 end

  $win.details configure -text ""

  if {$fname == ""} {
    set any [call_act_r_command "history-data-available" $model [list "bold-prediction-with-time"]]

    if {$any == "nil" || $any == ""} {
      $win.details configure -text "No Data available"
      set data [list "nothing"]
    } else {
      $win.details configure -text "Collecting Data ..."

      set id [call_act_r_command "start-incremental-history-data" $model [list "bold-prediction-with-time" 16000]]
 
      if {$id != ""} {
        set data [get_incremental_history $id]
      } else {
        tk_messageBox -icon warning -type ok -title "Get BOLD History warning" \
                      -message "Unknown problem occurred trying to get data."
        $win.details configure -text "Data Error occurred"

        $win.buttons.zoom_in configure -state normal
        $win.buttons.zoom_out configure -state normal
        $win.buttons.grid0 configure -state normal
        $win.buttons.grid1 configure -state normal
        $win.buttons.grid2 configure -state normal
        $win.get configure -state normal
        $win.save configure -state normal
        $win.load configure -state normal

        return "" 
      }

      $win.details configure -text "BOLD data for model $model"
    }
  } else {
    global bold_graph_history_warnings
    set bold_graph_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "bold_graph_record_warnings" "Environment command for capturing warnings during Load BOLD data."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    $win.details configure -text "Loading Data from file $fname"

    set id [call_act_r_command "start-incremental-history-data" $model [list "bold-prediction-with-time" 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load BOLD problem" \
                    -message "Get-history-data returned failure result.\n$bold_graph_history_warnings."
  
      $win.details configure -text "Failure to load data"

      $win.buttons.zoom_in configure -state normal
      $win.buttons.zoom_out configure -state normal
      $win.buttons.grid0 configure -state normal
      $win.buttons.grid1 configure -state normal
      $win.buttons.grid2 configure -state normal
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

    upvar #0 $win.data d
    unset d

    global bold_color_list

    set d(times) [lrange [lindex $data 0] 1 end]

    set i 0

    set max 0

    foreach b [lrange $data 1 end] {

      set buf [lindex $b 0]

      $list_box insert end $buf

      $list_box itemconfigure $i -selectbackground [lindex $bold_color_list $i]

      set d($buf) [lrange $b 1 end]
      set d($buf.color) [lindex $bold_color_list $i]

      set m 0
      foreach z [lrange $b 1 end] {
      
       if {$z > $m} {set m $z}
      }
      
      if {$m == 0} {
        set d($buf.max) 1
      } else {
        set d($buf.max) $m
      }

      if {$m > $max} {set max $m}
      incr i
    }

    if {$max == 0} {
      set d(max) 1
    } else {
      set d(max) $max
    }
  } else {
    # should have more error checking...  
  }
  
  $win.buttons.zoom_in configure -state normal
  $win.buttons.zoom_out configure -state normal
  $win.buttons.grid0 configure -state normal
  $win.buttons.grid1 configure -state normal
  $win.buttons.grid2 configure -state normal
  $win.get configure -state normal
  $win.save configure -state normal
  $win.load configure -state normal


  if {$fname == "" && $data != "nothing"} {
    $win.details configure -text "BOLD data for model $model"
  } elseif {$fname != ""} {
    $win.details configure -text $comment
  }
}

add_bold_trace_type "graph" make_bold_multi_graphs
