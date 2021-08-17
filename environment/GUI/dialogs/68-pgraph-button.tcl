

proc select_pgraph {} {

  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Production Graph" -message "Production Graph viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .pgraph]]

    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .pgraph $win]

    frame $win.frame -borderwidth 0  
    
    frame $win.frame.f -borderwidth 0

    canvas $win.frame.f.canvas  \
           -xscrollcommand "$win.frame.f.scrlx set" \
           -yscrollcommand "$win.frame.f.scrly set" \
           -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
       
    scrollbar $win.frame.f.scrlx \
              -command "$win.frame.f.canvas xview" -orient horizontal

    scrollbar $win.frame.f.scrly \
              -command "$win.frame.f.canvas yview" -orient vertical

    global $win.count
    global $win.max
    global $win.data

    global $win.labels
    global $win.boxes
    global $win.links
    global $win.graph_name
 
    set $win.count ""
    set $win.max ""
    set $win.data ""

    set $win.labels ""
    set $win.boxes ""
    set $win.links ""
    set $win.graph_name ""
 
    global $win.fname
    set $win.fname ""

    call_act_r_command "record-history" $model [list "production-history"]

    bind $win.frame.f.canvas <Destroy> "
      call_act_r_command stop-recording-history $model [list production-history]
      global $win.count
      unset $win.count
      global $win.max
      unset $win.max  
      global $win.data
      unset $win.data
      global $win.labels
      unset $win.labels
      global $win.boxes
      unset $win.boxes
      global $win.links
      unset $win.links
      global $win.graph_name
      unset $win.graph_name
    "

    
    label $win.text -font text_font -anchor nw
    label $win.notes -font text_font -anchor nw
  
    frame $win.buttons -borderwidth 0

    radiobutton $win.buttons.grid0 -text "All Transitions" -font button_font -value "production-graph-all" -variable $win.buttons.value
    radiobutton $win.buttons.grid1  -command "change_p_graph_data $win $model" -text "Frequencies" -font button_font -value "production-graph-freq" -variable $win.buttons.value
    radiobutton $win.buttons.grid2 -command "change_p_graph_data $win $model" -text "Cycles" -font button_font -value "production-graph-cycles" -variable $win.buttons.value
    radiobutton $win.buttons.grid3 -command "change_p_graph_data $win $model" -text "Unique Cycles" -font button_font -value "production-graph-u-cycles" -variable $win.buttons.value
    radiobutton $win.buttons.grid4 -command "change_p_graph_data $win $model" -text "Runs" -font button_font -value "production-graph-runs" -variable $win.buttons.value
    radiobutton $win.buttons.grid5 -command "change_p_graph_data $win $model" -text "Unique Runs" -font button_font -value "production-graph-u-runs" -variable $win.buttons.value
    radiobutton $win.buttons.grid6 -command "change_p_graph_data $win $model" -text "Utilities" -font button_font -value "production-graph-utility" -variable $win.buttons.value

    checkbutton $win.buttons.check \
                -text "Hide unused productions" \
                -font checkbox_font \
                -variable $win.check_val \
                -command "toggle_graph_unused $win $model" \
                -onvalue 1 -offvalue 0


    label $win.frame.cur_d -font text_font -anchor center 
    label $win.frame.of -font text_font -text "of" -anchor center 
    label $win.frame.max_d -font text_font -anchor center 

  
    button $win.frame.next -command [list pgraph_flip_button_action increment_p_graph_count $win $model] -text "->" -font graphic_trace_font -state disable 
    button $win.frame.previous -command [list pgraph_flip_button_action decrement_p_graph_count $win $model] -text "<-" -font graphic_trace_font -state disable

    button $win.save1 -command "save_phistory_graph $win" -text "Save as .eps" -font button_font
    button $win.save2 -command "save_phistory_graph_dot $win" -text "Save as .dot" -font button_font


    button $win.get -text "Get History" -font button_font -command "get_p_graph_data $win $model"
    button $win.save -text "Save History" -font button_font -command "save_p_graph_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_p_graph_history_data $win $model"


    global $win.labels
    global $win.boxes
    global $win.links
    global $win.graph_name 


    place $win.buttons.grid0 -x 0 -y 0 -height 24
    place $win.buttons.grid1  -x 0 -y 25 -height 24
    place $win.buttons.grid2 -x 0 -y 50 -height 24
    place $win.buttons.grid3 -x 0 -y 75 -height 24
    place $win.buttons.grid4 -x 0 -y 100 -height 24
    place $win.buttons.grid5 -x 0 -y 125 -height 24
    place $win.buttons.grid6 -x 0 -y 150 -height 24

    place $win.buttons.check -x 0 -rely 1.0 -y -25 -height 24


    pack $win.frame.f.scrlx -side bottom -fill x
    pack $win.frame.f.scrly -side right -fill y
    pack $win.frame.f.canvas -side left -fill both

    place $win.frame.f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -25

    place $win.frame.of -relx .5 -x -15 -width 30 -rely 1.0 -y -25 -height 24
    place $win.frame.cur_d -relx .5 -x -77 -width 60 -rely 1.0 -y -25 -height 24
    place $win.frame.previous -relx .5 -x -109 -width 30 -rely 1.0 -y -25 -height 24
    place $win.frame.max_d -relx .5 -x 17 -width 60 -rely 1.0 -y -25 -height 24
    place $win.frame.next -relx .5 -x 79 -width 30 -rely 1.0 -y -25 -height 24

 
    place $win.buttons -x 0 -y 0 -width 150 -relheight 1.0 -height -100
    place $win.frame -x 150 -y 0 -relwidth 1.0 -relheight 1.0 -height -75 -width -150

    place $win.save1 -x 0 -rely 1.0 -y -75 -width 100 -height 24
    place $win.save2 -x 0 -rely 1.0 -y -50 -width 100 -height 24

    
    place $win.text -x 100 -rely 1.0 -y -75 -relwidth 1.0 -width -100 -height 24
    place $win.notes -x 100 -rely 1.0 -y -50 -relwidth 1.0 -width -100 -height 24

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

    $win.buttons.grid0 invoke

    $win.buttons.grid0 configure -command "change_p_graph_data $win $model"

    wm deiconify $win

  }  
} 

proc pgraph_flip_button_action {action win model} {

  $action $win
  draw_p_graph $win $model 0
}
                       

proc increment_p_graph_count {win} {
  upvar #0 $win.count count
  upvar #0 $win.max max

  if {$count != $max} {
    incr count 1
  }
}

proc decrement_p_graph_count {win} {
  upvar #0 $win.count count

  if {$count != 0} {
    incr count -1
  }
}


proc load_p_graph_history_data {win model} {

  global top_dir

  set fname [tk_getOpenFile -title "Load Production History" -initialdir $top_dir] 

  if {$fname != ""} {
    draw_p_graph $win $model 1 $fname
  }
}


proc toggle_graph_unused {win model} {
  draw_p_graph $win $model 0
}


set pg_history_warnings ""

proc pg_record_warnings {model s} {
  global pg_history_warnings

  set pg_history_warnings "$pg_history_warnings$s"
  return ""
}


proc get_p_graph_data {win model} {

  global $win.fname
  set $win.fname ""

  draw_p_graph $win $model 1
}



proc change_p_graph_data {win model} {

  upvar #0 $win.fname saved_fname

  draw_p_graph $win $model 1 $saved_fname
}


proc draw_p_graph {win model clear_all {fname ""}} {

  $win.buttons.grid0 configure -state disabled
  $win.buttons.grid1 configure -state disabled
  $win.buttons.grid2 configure -state disabled
  $win.buttons.grid3 configure -state disabled
  $win.buttons.grid4 configure -state disabled
  $win.buttons.grid5 configure -state disabled
  $win.buttons.grid6 configure -state disabled
  $win.frame.next configure -state disabled
  $win.frame.previous configure -state disabled
  $win.save1 configure -state disabled
  $win.save2 configure -state disabled
  $win.get configure -state disabled
  $win.save configure -state disabled
  $win.load configure -state disabled


  upvar #0 $win.data data
  upvar #0 $win.count count
  upvar #0 $win.max max
  upvar #0 $win.buttons.value value
  upvar #0 $win.check_val hide_unused

  upvar #0 $win.labels labels
  upvar #0 $win.boxes boxes
  upvar #0 $win.links links
  upvar #0 $win.graph_name name

  upvar #0 $win.fname saved_fname

  $win.frame.f.canvas delete all 

  if {$clear_all == 1} {

    $win.notes configure -text ""
    $win.frame.cur_d configure -text ""
    $win.frame.max_d configure -text ""


    if {$fname == ""} {
      set any [call_act_r_command "history-data-available" $model [list "production-history"]]

      if {$any == "nil" || $any == ""} {
        $win.text configure -text "No Data available"
      } else {
        $win.text configure -text "Collecting Data ..."
        set id [call_act_r_command "start-incremental-history-data" $model [list $value 16000]]
   
        if {$id != ""} {
          set data [get_incremental_history $id]
        } else {
          tk_messageBox -icon warning -type ok -title "Get Text History warning" \
                        -message "Unknown problem occurred trying to get data."
          $win.buttons.grid0 configure -state normal
          $win.buttons.grid1 configure -state normal
          $win.buttons.grid2 configure -state normal
          $win.buttons.grid3 configure -state normal
          $win.buttons.grid4 configure -state normal
          $win.buttons.grid5 configure -state normal
          $win.buttons.grid6 configure -state normal
          $win.save1 configure -state normal
          $win.save2 configure -state normal
          $win.get configure -state normal
          $win.save configure -state normal
          $win.load configure -state normal
      
          set name "Data Error occurred"
          return "" 
        }

        $win.text configure -text "Production Graph data for model $model"
        set name "Production Graph data for model $model"
        set count 1
        set max [expr [llength $data] - 1]
      }
    } else {
      global pg_history_warnings

      set pg_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "pg_record_warnings" "Environment command for capturing warnings during Load Production Graph."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      $win.text configure -text "Loading Data from file $fname"

      set id [call_act_r_command "start-incremental-history-data" $model [list $value 16000 $fname]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {$id == "" || $id == "null"} {
        tk_messageBox -icon warning -type ok -title "Load Production Graph History problem" \
                    -message "Get-history-data returned failure result.\n$pg_history_warnings."
        set name ""
        $win.text configure -text ""

        $win.buttons.grid0 configure -state normal
        $win.buttons.grid1 configure -state normal
        $win.buttons.grid2 configure -state normal
        $win.buttons.grid3 configure -state normal
        $win.buttons.grid4 configure -state normal
        $win.buttons.grid5 configure -state normal
        $win.buttons.grid6 configure -state normal
        $win.save1 configure -state normal
        $win.save2 configure -state normal
        $win.get configure -state normal
        $win.save configure -state normal
        $win.load configure -state normal

        return ""
      } else {

        set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]

        set data [get_incremental_history $id]


        set name [json::json2dict [lindex $comment 0]]

        $win.text configure -text "Loaded Data from file $fname"

        set saved_fname $fname
        set count 1
        set max [expr [llength $data] - 1]
      }
    }   
  }

  set labels [list]
  set boxes [list]
  set links [list]

  set min_time ""
  set max_time ""

  set size [lindex $data 0]

  if {[lindex $size 0] == "size"} {
    $win.frame.f.canvas configure -width [lindex $size 1] -height [lindex $size 2] -scrollregion "0 0 [lindex $size 1] [lindex $size 2]"
  } else {
# should have more error checking...  
  }

  foreach x [lindex $data $count] {
    switch [lindex $x 0] {
      label { 
        set p [lindex $x 1]

        if {[lindex $x 8] == "gray"} {
          lappend labels "\"$p\" \[ color = gray \]"
        } else {
          lappend labels "\"$p\""
        }

        if {$hide_unused == 0 || [lindex $x 8] == "black"} {
          $win.frame.f.canvas create text [lindex $x 2] [lindex $x 3] -anchor center -font text_font -text $p -tag $p
          $win.frame.f.canvas bind $p <ButtonPress> "p_graph_p_view $win $p {[lindex $x 9]}"
          $win.frame.f.canvas create rectangle [lindex $x 4] [lindex $x 5] [lindex $x 6] [lindex $x 7] -outline [lindex $x 8] -width 2 -tag $p
        }
      }

      box { 
        lappend boxes "\"[lindex $x 1]\" \[ color = [lindex $x 6] \]"
        
        if {$hide_unused == 0 || [lindex $x 7] == 1} {
          $win.frame.f.canvas create rectangle [lindex $x 2] [lindex $x 3] [lindex $x 4] [lindex $x 5] -outline [lindex $x 6] -width 2 
        }
      }
      link {
        if {[lindex $x 3] == "gray"} {
          lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\" \[ style=dashed color=gray \]"

          if {$hide_unused == 0 || [lindex $x 5] == 1} {
            $win.frame.f.canvas create line [lindex $x 6] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -dash ".-"
          }
        } else {
          lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\""

          if {$hide_unused == 0 || [lindex $x 5] == 1} {
            $win.frame.f.canvas create line [lindex $x 6] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -tag transient
          }
        }
      }  
      min_time {
        set min_time [lindex $x 1]
      }
      max_time {
        set max_time [lindex $x 1]
      }
    }
  }
  

  $win.buttons.grid0 configure -state normal
  $win.buttons.grid1 configure -state normal
  $win.buttons.grid2 configure -state normal
  $win.buttons.grid3 configure -state normal
  $win.buttons.grid4 configure -state normal
  $win.buttons.grid5 configure -state normal
  $win.buttons.grid6 configure -state normal
  $win.save1 configure -state normal
  $win.save2 configure -state normal
  $win.get configure -state normal
  $win.save configure -state normal
  $win.load configure -state normal


  if {$max > 1} {
    if {$count < $max} {
      $win.frame.next configure -state normal
    }
    if {$count > 1} {
      $win.frame.previous configure -state normal
    }
  }
 
  if {$min_time != "" && $max_time != ""} {
    $win.notes configure -text "Time [expr $min_time / 1000.0] - [expr $max_time / 1000.0]"
  } else {
    $win.notes configure -text ""
  }

 $win.frame.cur_d configure -text $count
 $win.frame.max_d configure -text $max
}




proc save_p_graph_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save Production History" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "production-history"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save Production History warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global pg_history_warnings

      set pg_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "pg_record_warnings" "Environment command for capturing warnings during Save Production History."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "production-history" $fname "Data saved from Production History window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save Production History error" \
                      -message "Get-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save Production History problem" \
                      -message "Get-history-data returned failure result.\n$pg_history_warnings."
      }
    }
  }
}


proc p_graph_p_view {win name text} {

  # make a new window 
  set top [toplevel [new_variable_name ".p_graph_p_view"]]

  wm geometry $top [get_configuration .p_graph_p_view $top]

  wm title $top "[wm title $win] Production: $name"     

  frame $top.frame -borderwidth 0  
    
  set text_box [text $top.frame.text -font text_font \
                                     -yscrollcommand "$top.frame.scrl set" ]

  $text_box insert 0.0 $text

  set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
  place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
  pack $scrl_bar -side right -fill y 
  pack $text_box -side left -expand 1 -fill both
 
}



proc save_phistory_graph {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {
    $win.frame.f.canvas postscript -file $fname -width [$win.frame.f.canvas cget -width] -height [$win.frame.f.canvas cget -height] -x 0 -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.f.canvas cget -height] -pagewidth [$win.frame.f.canvas cget -width]
  }
}


proc save_phistory_graph_dot {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"DOT file" "*.dot"}}]

  if {$fname != ""} {

    upvar #0 $win.labels labels
    upvar #0 $win.boxes boxes
    upvar #0 $win.links links
    upvar #0 $win.graph_name name
 
    write_data "digraph $name \{\n" $fname

    foreach label $labels {
      append_data "  $label ;\n" $fname
    }

    foreach box $boxes {
      append_data "  $box ;\n" $fname
    }

    foreach link $links {
      append_data "  $link ;\n" $fname
    }

    append_data "\}" $fname
  }
}


add_p_history_type "graph" select_pgraph
