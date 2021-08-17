 
proc select_graphic_trace {} {

  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Graphic Trace" -message "Graphic trace viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .graphic_trace]]

    global $win.scale

    wm withdraw $win

    wm geometry $win [get_configuration .graphic_trace $win]

    record_new_window $win $win $model

    call_act_r_command "record-history" $model [list "buffer-trace"]

    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
           -xscrollcommand "$win.frame.scrl set" \
           -width 2000 -height 400 -scrollregion {-4 0 2000 400} -bg white
          
    scrollbar $win.frame.scrl \
              -command "$win.frame.canvas xview" -orient horizontal

    set $win.scale 1.0

    bind $win.frame.canvas <Destroy> "
      call_act_r_command stop-recording-history $model [list buffer-trace]
      global $win.scale
      unset $win.scale
    "


    canvas $win.canvas1 -width 150 -height 400 -bg white

    label $win.details -font label_font 

    label $win.time_label -font label_font -text "Time:"
    label $win.time -font text_font    

    label $win.duration_label -font label_font -text "Duration:"
    label $win.duration -font text_font    

    label $win.notes_label -font label_font -text "Notes:"
    label $win.notes -font text_font -anchor nw
  
    label $win.request_label -font label_font -text "Request:"
    label $win.request -font text_font -anchor nw
  
    label $win.chunk_label -font label_font -text "Chunk:"
    label $win.chunk -font text_font -anchor nw
  
    frame $win.buttons -borderwidth 0

    button $win.buttons.zoom_in -command "horiz_zoom_in $win" -text "+" -font button_font
    button $win.buttons.zoom_out -command  "horiz_zoom_out $win" -text "-" -font button_font


    checkbutton $win.buttons.ht \
                -text "Hide text" \
                -font checkbox_font \
                -variable $win.ht \
                -command "configure_h_trace_text $win" \
                -onvalue 1 -offvalue 0

    checkbutton $win.buttons.hg \
                -text "Hide grid" \
                -font checkbox_font \
                -variable $win.hg \
                -command "draw_h_grid $win" \
                -onvalue 1 -offvalue 0

    button $win.buttons.save1 -command "save_horiz_graphic_trace $win" -text "Save .eps" -font button_font
    button $win.buttons.save2 -command "save_horiz_graphic_trace_multi $win" -text "Save .ps" -font button_font


    button $win.get -text "Get History" -font button_font -command "get_h_graph_data $win $model"
    button $win.save -text "Save History" -font button_font -command "save_h_graph_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_h_graph_history_data $win $model"


    pack $win.frame.scrl -side bottom -fill x
    pack $win.frame.canvas -side top -fill both 

    place $win.canvas1  -x 0 -y 0 -width 150 -relheight 1.0 -height -175
    place $win.frame -x 150 -y 0 -relwidth 1.0 -width -150 -relheight 1.0 -height -175
  
    place $win.details -x 0 -rely 1.0 -y -175 -height 24 -relwidth 1.0
    
    place $win.request_label -x 0 -rely 1.0 -y -150 -width 70 -height 24
    place $win.request -x 71 -rely 1.0 -y -150 -relwidth 1.0 -width -70 -height 24

    place $win.chunk_label -x 0 -rely 1.0 -y -125 -width 70 -height 24
    place $win.chunk -x 71 -rely 1.0 -y -125 -relwidth 1.0 -width -70 -height 24

    place $win.notes_label -x 0 -rely 1.0 -y -100 -width 70 -height 24
    place $win.notes -x 71 -rely 1.0 -y -100 -relwidth 1.0 -width -70 -height 24

    place $win.time_label -x 0 -rely 1.0 -y -75 -width 70 -height 24
    place $win.time -x 71 -rely 1.0 -y -75 -relwidth 0.5 -width -70 -height 24

    place $win.duration_label -relx 0.5 -rely 1.0 -y -75 -width 70 -height 24
    place $win.duration -relx 0.5 -x 71 -rely 1.0 -y -75 -relwidth 0.5 -width -70 -height 24

    place $win.buttons.zoom_in -x 0 -y 0 -width 59 -height 24
    place $win.buttons.zoom_out -x 60 -y 0 -width 59 -height 24

    place $win.buttons.ht -x 320 -y 0 -width 99 -height 24
    place $win.buttons.hg -x 420 -y 0 -width 99 -height 24

    place $win.buttons.save1 -x 140 -y 0 -width 79 -height 24
    place $win.buttons.save2 -x 220 -y 0 -width 79 -height 24

    place $win.buttons -x 0 -rely 1.0 -y -50 -width 525 -height 25

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

    # now show the window 

    $win.frame.canvas xview moveto 0

    wm deiconify $win

    pick_buffers

    return $win
  }
} 

proc horiz_zoom_out {win} {

  upvar #0 $win.scale scale

  set scale [expr .5 * $scale]
 
  $win.frame.canvas scale trace_items 0 0 0.5 1.0
  $win.frame.canvas configure -scrollregion "-4 0 [expr .5 * [lindex [$win.frame.canvas cget -scrollregion] 2]] 400"
  $win.frame.canvas configure -width [expr .5 * [$win.frame.canvas cget -width]]

  foreach x [$win.frame.canvas find withtag trace_text] {
    $win.frame.canvas itemconfigure $x -width [expr .5 * [$win.frame.canvas itemcget $x -width]]
  }
}

proc horiz_zoom_in {win} {

  upvar #0 $win.scale scale

  if {$scale < 16} {
    set scale [expr 2 * $scale]

    $win.frame.canvas scale trace_items 0 0 2.0 1.0
    $win.frame.canvas configure -scrollregion "-4 0 [expr 2 * [lindex [$win.frame.canvas cget -scrollregion] 2]] 400"
    $win.frame.canvas configure -width [expr 2 * [$win.frame.canvas cget -width]]
      
    foreach x [$win.frame.canvas find withtag trace_text] {
      $win.frame.canvas itemconfigure $x -width [expr 2 * [$win.frame.canvas itemcget $x -width]]
    }
  }
}



proc get_h_graph_data {win model} {

  draw_horiz_items $win $model ""
}

proc load_h_graph_history_data {win model} {

  global top_dir

  set fname [tk_getOpenFile -title "Load Production History" -initialdir $top_dir] 

  if {$fname != ""} {
    draw_horiz_items $win $model $fname
  }
}



set h_graph_history_warnings ""

proc h_graph_record_warnings {model s} {
  global h_graph_history_warnings

  set h_graph_history_warnings "$h_graph_history_warnings$s"
  return ""
}


proc save_h_graph_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save Graphic Trace Data" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "buffer-trace"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save Graphic Trace warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global h_graph_history_warnings

      set h_graph_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "h_graph_record_warnings" "Environment command for capturing warnings during Save Horizontal Graphic trace."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "buffer-trace" $fname "Data saved from Graphic Trace window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save Graphic Trace error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save Graphic Trace problem" \
                      -message "Save-history-data returned failure result.\n$h_graph_history_warnings."
      }
    }
  }
}

proc draw_h_grid {win} {
  upvar #0 $win.hg hide
  upvar #0 $win.scale scale

  if {$hide == 0} {
    set max [$win.frame.canvas cget -width]

    set x 0

    while {$x <= $max} {
      set x_pos [expr $x * $scale]
      $win.frame.canvas create line $x_pos 0 $x_pos 400 -width 1 -f gray -tag [list grid trace_items]
      $win.frame.canvas create text $x_pos 400 -text [format "%.3f" [expr 0.001 * $x]] -anchor sw -font graphic_trace_font -tag [list grid trace_items]
      incr x 50
    }
    $win.frame.canvas lower grid 

  } else {
    $win.frame.canvas delete grid
  }
} 


proc configure_h_trace_text {win} {
  upvar #0 $win.ht hide

  if {[$win.frame.canvas find withtag trace_text] != ""} {
    if {$hide == 0} {

      $win.frame.canvas itemconfigure trace_text -fill black
      $win.frame.canvas raise trace_text boxes
    } else {
      $win.frame.canvas itemconfigure trace_text -fill white
      $win.frame.canvas lower trace_text all
    }
  } 
}


set h_graphic_trace_color_list [list #ff0000 #8080ff #008000 #ff8000 #7f7f7f #ff00ff #ffff00 #c08000 #f0f0f0 #c000c0 #00c050 #f0c0ff #c05850 #50ff25]

proc draw_horiz_items {win model fname} { 

  $win.buttons.zoom_in configure -state disabled
  $win.buttons.zoom_out configure -state disabled
  $win.buttons.ht configure -state disabled
  $win.buttons.hg configure -state disabled
  $win.buttons.save1 configure -state disabled
  $win.buttons.save2 configure -state disabled
  $win.get configure -state disabled
  $win.save configure -state disabled
  $win.load configure -state disabled

  $win.frame.canvas delete all 
  $win.canvas1 delete all

  $win.details configure -text ""
  $win.request configure -text ""
  $win.chunk configure -text ""
  $win.notes configure -text ""
  $win.time configure -text ""
  $win.duration configure -text ""

  if {$fname == ""} {
    set any [call_act_r_command "history-data-available" $model [list "buffer-trace"]]

    if {$any == "nil" || $any == ""} {
      $win.details configure -text "No Data available"
      set data [list "nothing"]
    } else {
      $win.details configure -text "Collecting Data ..."
      set id [call_act_r_command "start-incremental-history-data" $model [list "horizontal-graphic-trace" 16000]]
 
      if {$id != ""} {
        set data [get_incremental_history $id]
      } else {
        tk_messageBox -icon warning -type ok -title "Get Graphic trace History warning" \
                      -message "Unknown problem occurred trying to get data."
        $win.details configure -text "Data Error occurred"
        $win.buttons.zoom_in configure -state normal
        $win.buttons.zoom_out configure -state normal
        $win.buttons.ht configure -state normal
        $win.buttons.hg configure -state normal
        $win.buttons.save1 configure -state normal
        $win.buttons.save2 configure -state normal
        $win.get configure -state normal
        $win.save configure -state normal
        $win.load configure -state normal

        return "" 
      }

      $win.details configure -text "Graphic Trace data for model $model"
    }
  } else {
    global h_graph_history_warnings
    set h_graph_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "h_graph_record_warnings" "Environment command for capturing warnings during Load Horizontal Graphic Trace."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    $win.details configure -text "Loading Data from file $fname"

    set id [call_act_r_command "start-incremental-history-data" $model [list "horizontal-graphic-trace" 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load Graph Trace problem" \
                    -message "Get-history-data returned failure result.\n$h_graph_history_warnings."
  
      $win.details configure -text "Failure to load data"

      $win.buttons.zoom_in configure -state normal
      $win.buttons.zoom_out configure -state normal
      $win.buttons.ht configure -state normal
      $win.buttons.hg configure -state normal
      $win.buttons.save1 configure -state normal
      $win.buttons.save2 configure -state normal
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

  set size [lindex $data 0]

  upvar #0 $win.scale scale

  set scale 1.0

  if {[lindex $size 0] == "size"} {

    set max [lindex $size 1]
    $win.frame.canvas configure -width [expr $max + 5]
    $win.frame.canvas configure -scrollregion "-4 0 [expr 1 + $max] 400"
 
    draw_h_grid $win

  } else {
    # should have more error checking...  
  }


  global h_graphic_trace_color_list

  foreach x [lrange $data 1 end] {
    switch [lindex $x 0] {
      label { 
        $win.frame.canvas create text -145 [lindex $x 2] -anchor w -font text_font -justify right -text [lindex $x 1] -width 140 -fill [lindex $h_graphic_trace_color_list [lindex $x 3]]
        $win.canvas1 create text 5 [lindex $x 2] -anchor w -font text_font -justify right -text [lindex $x 1] -width 140 -tag label_tag -fill [lindex $h_graphic_trace_color_list [lindex $x 3]]
      }

      rectangle {
        set box_name [new_variable_name box]
 
        $win.frame.canvas create rectangle [lindex $x 1] [lindex $x 2] [lindex $x 3] [lindex $x 4] -width 1 -fill [lindex $h_graphic_trace_color_list [lindex $x 5]] -outline black -tag [list boxes trace_items $box_name]

        set duration [format "%.3f" [expr 0.001 * ([lindex $x 3] - [lindex $x 1])]]
        set time [format "%.3f - %.3f" [expr 0.001 * [lindex $x 1]] [expr 0.001 * [lindex $x 3]]] 
 
        $win.frame.canvas bind $box_name <Enter> "$win.request configure -text {[lindex $x 6]}
                                                  $win.chunk configure -text {[lindex $x 7]}
                                                  $win.notes configure -text {[lindex $x 8]}
                                                  $win.time configure -text {$time}
                                                  $win.duration configure -text $duration
                                                 "

        $win.frame.canvas bind $box_name <Leave> "$win.request configure -text \"\"
                                                  $win.chunk configure -text \"\"
                                                  $win.notes configure -text \"\"
                                                  $win.time configure -text \"\"
                                                  $win.duration configure -text \"\"
                                                 "

# For now don't allow clicking to open other dialogs because of potential problems  $win.frame.canvas bind $box_name <ButtonPress> [list history_custom_buffer_view $win $key $current [lindex $x 10] [lindex $x 1] [lindex $x 3]]

        if {[lindex $x 9] != ""} {
          $win.frame.canvas create text [expr 2 + [lindex $x 1]] [lindex $x 2] -text [lindex $x 9] -anchor nw -font graphic_trace_font -fill black -tag [list trace_items trace_text $box_name] -width [expr [lindex $x 3] - [lindex $x 1]]
        }
      }
    }
  }

  configure_h_trace_text $win
  
  $win.buttons.zoom_in configure -state normal
  $win.buttons.zoom_out configure -state normal
  $win.buttons.ht configure -state normal
  $win.buttons.hg configure -state normal
  $win.buttons.save1 configure -state normal
  $win.buttons.save2 configure -state normal
  $win.get configure -state normal
  $win.save configure -state normal
  $win.load configure -state normal

# looks better at 2 pixels per ms, but easier to draw at 1

  horiz_zoom_in $win

  if {$fname == "" && $data != "nothing"} {
    $win.details configure -text "Graphic Trace data for model $model"
  } elseif {$fname != ""} {
    $win.details configure -text $comment
  }

}


proc save_horiz_graphic_trace {win} {
  set fname [tk_getSaveFile -title "Save graphic trace as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {
    $win.frame.canvas postscript -file $fname -width [expr 150 + [$win.frame.canvas cget -width]] -height [$win.frame.canvas cget -height] -x -150 -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.canvas cget -height] -pagewidth [expr 150 + [$win.frame.canvas cget -width]]
  }
}

proc save_horiz_graphic_trace_multi {win} {
  set fname [tk_getSaveFile -title "Save graphic trace as" -filetypes {{"PostScript" "*.ps"}}]

  if {$fname != ""} {  
 
   set width 1400.0
   set height 400

   set xMax [expr 150.0 + [$win.frame.canvas cget -width]]
   set NOP [expr ceil ($xMax / $width)]   
  
# The following code was modified from code written by Robert Heller
# in a file called bridge.tcl which was posted to comp.lang.tcl as
# an example of producing multi-page ps files.

   set prFile [open $fname w]

  puts $prFile "%!PS-Adobe-2.0"
  puts $prFile "%%Creator: ACT-R Environment Copyright 2007 Dan Bothell"
  puts $prFile "%%Title: Horizontal Graphic Trace"
  puts -nonewline $prFile "%%CreationDate: "
  global tcl_version
  if {$tcl_version >= 7.6} {
    puts $prFile "[clock format [clock seconds]]"
  } else {
    puts $prFile "[exec date]"
  }
  puts $prFile "%%Pages: $NOP $xMax $width [expr ceil($xMax / $width)]"
  puts $prFile "%%EndComments"
  puts $prFile "/EncapDict 200 dict def EncapDict begin"
  puts $prFile "/showpage {} def /erasepage {} def /copypage {} def end"
  puts $prFile "/BeginInclude {0 setgray 0 setlinecap 1 setlinewidth"
  puts $prFile "0 setlinejoin 10 setmiterlimit \[\] 0 setdash"
  puts $prFile "/languagelevel where {"
  puts $prFile "  pop"
  puts $prFile "  languagelevel 2 ge {"
  puts $prFile "    false setoverprint"
  puts $prFile "    false setstrokeadjust"
  puts $prFile "  } if"
  puts $prFile "} if"
  puts $prFile "newpath"
  puts $prFile "save EncapDict begin} def"
  puts $prFile "/EndInclude {restore end} def"
  puts $prFile "%%EndProlog"
  set pageNo 1


  for {set xoff 0} {$xoff < $xMax} {set xoff [expr $xoff + $width]} {

      puts $prFile "%%Page: $pageNo $pageNo"
      puts $prFile "BeginInclude"

      # this one works set eps "[$win.frame.canvas postscript -height $height -width $width -x [expr $xoff - 150] -y 0 -pageanchor nw -pagex 0.25i -pagey 7.5i -pagewidth 8.0i]"

      set eps "[$win.frame.canvas postscript -height $height -width $width -x [expr $xoff - 150] -y 0 -pageanchor nw -pagex 2.0i -pagey 0.5i -pagewidth 10.0i -rotate 1]"

 
      set EOC [string first "%%BeginProlog\n" "$eps"]
      set EOF [expr [string first "%%EOF\n" "$eps"] - 1]

      puts $prFile "[horiz_StripPSComments [string range $eps $EOC $EOF]]"
      puts $prFile "EndInclude showpage"
      incr pageNo
  }
  puts $prFile "%%EOF"
  close $prFile
  }
}

proc horiz_StripPSComments {PSString} {
  set result {}
  foreach l [split "$PSString" "\n"] {
    set i [string first "%" "$l$"]
    if {$i == 0} {
      set result "$result\n"
    } elseif {$i > 0 && [regexp {(^.*[^\\])(%.*$)} "$l" whole prefix comment]} {
      set result "$result$prefix\n"
    } else {
      set result "$result$l\n"
    }
  }
  return "$result"
}

add_graphic_trace_type horizontal select_graphic_trace
