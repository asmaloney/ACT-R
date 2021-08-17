
proc select_ptrace {} {
    
  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Production History" -message "Production history viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .ptrace]]

    global $win.scale
    global $win.grid_state
    global $win.c_height
    global $win.c_width
    global $win.label_offset
    global $win.data

    wm withdraw $win

    wm geometry $win [get_configuration .ptrace $win]

    record_new_window $win $win $model

    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
           -xscrollcommand "$win.frame.scrlx set" \
           -yscrollcommand "$win.frame.scrly set" \
           -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
    canvas $win.frame.canvas1  \
           -yscrollcommand "$win.frame.scrly set" \
           -width 150 -height 300 -scrollregion {0 0 150 300} -bg white
   
       
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "scroll_ptraces_canvas $win" -orient vertical


    set $win.scale 1.0
          
    label $win.notes -font text_font  -textvariable $win.notesvar -anchor nw
  
    set $win.notesvar ""

    label $win.details -font text_font -textvariable $win.details -anchor nw
  
    set $win.details ""

    button $win.grid -command "p_trace_grid $win" -text "Grid" -font button_font

    button $win.zoom_in -command "p_trace_zoom_in $win" -text "+" -font button_font

    button $win.zoom_out -command  "p_trace_zoom_out $win" -text "-" -font button_font

    button $win.save1 -command "save_phistory_trace $win" -text "Save .eps" -font button_font
    button $win.save2 -command "save_phistory_trace_multi $win" -text "Save .ps" -font button_font


    # Add a checkbox to allow removing the empty columns to the tool

    checkbutton $win.check \
                -text "Hide empty columns" \
                -font checkbox_font \
                -variable $win.check_val \
                -command "draw_p_trace $win $model {} 1" \
                -onvalue 1 -offvalue 0

    button $win.get -text "Get History" -font button_font -command "draw_p_trace $win $model"
    button $win.save -text "Save History" -font button_font -command "save_p_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_p_history_data $win $model"


    canvas $win.key -width 300 -height 30 

    global gui_options

    $win.key create text 50 15 -anchor center -font label_font -text "Selected" -fill #FFF -tag label
    $win.key create text 150 15 -anchor center -font label_font -text "Matched" -fill #FFF -tag label
    $win.key create text 250 15 -anchor center -font label_font -text "Mismatched" -fill #FFF -tag label

    $win.key create rectangle 0 0 99 30 -fill $gui_options(p_selected) -width 0 -tag p_selected
    $win.key create rectangle 100 0 199 30 -fill $gui_options(p_matched) -width 0 -tag p_matched
    $win.key create rectangle 200 0 299 30 -fill $gui_options(p_mismatched) -width 0 -tag p_mismatched

    $win.key bind all <ButtonPress> "change_p_history_color %x $win.key $win.frame.canvas"
          
    $win.key raise label

    call_act_r_command record-history $model [list "production-history"]

    global $win.value
    set $win.value ""

    bind $win.frame.canvas <Destroy> "call_act_r_command stop-recording-history $model [list {production-history}]
                                      global $win.value
                                      unset $win.value" 


    set $win.grid_state 1

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    pack $win.frame.canvas1 -side left -fill y
    pack $win.frame.canvas -side left -fill both
 
    place $win.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -105
    place $win.key -x 0 -rely 1.0 -y -105 -height 30 -width 300
    place $win.details -x 300 -rely 1.0 -y -105 -height 30 -relwidth 1.0 -width -300

    place $win.notes -x 0 -rely 1.0 -y -75 -relwidth 1.0 -height 25

    place $win.grid -x 0 -rely 1.0 -y -50 -width 60 -height 24
    place $win.zoom_in -x 61 -rely 1.0 -y -50 -width 35 -height 24
    place $win.zoom_out -x 97 -rely 1.0 -y -50 -width 35 -height 24
    place $win.save1 -x 140 -rely 1.0 -y -50 -width 85 -height 24
    place $win.save2 -x 226 -rely 1.0 -y -50 -width 85 -height 24
    place $win.check -x 312 -rely 1.0 -y -50 -width 150 -height 24

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3

    wm deiconify $win

    return $win
  }
} 

proc change_p_history_color {x key display} {
  
  global gui_options
  
  if {$x < 100} {
    set index p_selected
    set label "Selected Production Color"
  } elseif {$x < 200} {
    set index p_matched
    set label "Matched Production Color"
  } else {
    set index p_mismatched
    set label "Mismatched Production Color"
  }  

  set new [tk_chooseColor -title $label -initialcolor $gui_options($index)]

  if {$new != ""} {
    set gui_options($index) $new
    $key itemconfigure $index -fill $new
    $display itemconfigure $index -fill $new
  }
}

proc scroll_ptraces_canvas {win args} {
   set ignore ""
   eval [append ignore $win.frame.canvas " " yview " " $args]
   set ignore ""
   eval [append ignore $win.frame.canvas1 " " yview " " $args]
}


set p_history_warnings ""

proc p_record_warnings {model s} {
  global p_history_warnings

  set p_history_warnings "$p_history_warnings$s"
  return ""
}

proc p_button_toggle {win state} {
  $win.grid configure -state $state
  $win.zoom_in configure -state $state
  $win.zoom_out configure -state $state
  $win.save1 configure -state $state
  $win.save2 configure -state $state
  $win.check configure -state $state
}

proc draw_p_trace {win model {file ""} {reuse 0}} {

  $win.frame.canvas delete all 
  $win.frame.canvas1 delete all
  
  global $win.check_val
 
  global $win.details

  upvar #0 $win.check_val hide_empty
  upvar $win.notesvar display
          
  upvar #0 $win.value old_data

  global $win.scale
  global $win.grid_state
  global $win.c_height
  global $win.c_width
  global $win.label_offset

  upvar $win.scale scale
  upvar $win.grid_state grid
  upvar $win.c_height g_c_height
  upvar $win.c_width g_c_width
  upvar $win.label_offset label_offset

  set c_height 0
  set c_width 0
  set t_height 0
  set t_width 0
  set n_width 0
  set x_display 0
   
  p_button_toggle $win disabled


  if {$file == ""} {
    if {$reuse == 1} {
      set result $old_data
    } else {

      set any [call_act_r_command "history-data-available" $model [list "production-history"]]

      if {$any == ""} {
        set $win.details "No Data available"
        p_button_toggle $win normal
        return ""
      } else {
        set $win.details "Collecting Data ..."
        


        set id [call_act_r_command "start-incremental-history-data" $model [list "production-grid" 16000]]

        if {$id == ""} { 
          set $win.details "No Data available"
          p_button_toggle $win normal
          return ""
        } else {

          set result [get_incremental_history $id]
          set $win.details "Data for model $model"
        }
      }
    }
  } else {
    global p_history_warnings

    set p_history_warnings ""
            
    set warning_monitor [add_new_cmd warning_monitor "p_record_warnings" "Environment command for capturing warnings during Load Production Grid."]

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    set $win.details "Loading Data from file $file"

    set id [call_act_r_command "start-incremental-history-data" $model [list "production-grid" 16000 $file]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load Retrieval History problem" \
                    -message "Get-history-data returned failure result.\n$p_history_warnings."
      set $win.details ""
      p_button_toggle $win normal
      return ""
    } else {

      set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]
      set result [get_incremental_history $id]

      set $win.details [json::json2dict [lindex $comment 0]]
    }
  }   

  set old_data $result

  foreach x $result {
    switch [lindex $x 1] {
      info { 

        set reasons [lindex $x 2]

        set c_height [expr [font metrics text_font -displayof $win.frame.canvas -linespace] + 3]

        set p_count [llength [lindex $x 3]]

        set c_count 0

        foreach i $result {
          if {[lindex $i 1] == "conflict-resolution"} {
            if {$hide_empty == 0 || [lindex $i 2] == true} {
              incr c_count
            }
          }
        }

        set c_width 100 
        set t_height [expr $c_height * (1 + $p_count)]
        set t_width [expr $c_count * $c_width]

        set n_width 0

        set names [list]

        foreach i [lindex $x 3] {
          set w [font measure text_font [lindex $i 0]]
          if {$w > $n_width} {
            set n_width $w
          }
          lappend names [lindex $i 0]
        }
          
        set n_width [expr $n_width + 10]

        set label_offset [expr -$n_width]

        $win.frame.canvas configure -width $t_width -height $t_height
        $win.frame.canvas1 configure -width $n_width -height $t_height
        $win.frame.canvas1 configure -scrollregion "0 0 $n_width $t_height"
        $win.frame.canvas configure -scrollregion "0 0 $t_width $t_height"

        set y $c_height
          
        foreach i [lindex $x 3] {
          
          set box_name [new_variable_name box]
 
          $win.frame.canvas create text [expr $label_offset + 5] $y -anchor nw -font text_font -text [lindex $i 0] 
          $win.frame.canvas1 create text 5 $y -anchor nw -font text_font -text [lindex $i 0] -tag $box_name
          $win.frame.canvas1 create line 0 $y $n_width $y -width 1 -f gray

          $win.frame.canvas1 bind $box_name <ButtonPress> "p_history_p_view $win [lindex $i 0] {[lindex $i 1]}"
        
          incr y $c_height
        }
      }

      conflict-resolution {
        
        if {$hide_empty == 0 || [lindex $x 2] == true} {
         $win.frame.canvas create text [expr $x_display + ($c_width / 2)] 0 -anchor n -font graphic_trace_font -text [expr [lindex $x 0] / 1000.0 ] -tag zoom

          set y $c_height

          global gui_options

          set index 0

          foreach i [lindex $x 3] {

            set box_name [new_variable_name box]
   
            switch [lindex $i 0] {
              0 {
                $win.frame.canvas create rectangle $x_display $y [expr $x_display + $c_width] [expr $y + $c_height] -width 0 -fill $gui_options(p_selected) -tag [list $box_name zoom p_selected]
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: [lindex $i 1]  U(n): [lindex $i 2]\""
                $win.frame.canvas bind $box_name <ButtonPress> "p_history_whynot_view $win 0 [lindex $i 1] [lindex $i 2] [lindex $names $index] [lindex $x 0]"
              }
              1 {
                $win.frame.canvas create rectangle $x_display $y [expr $x_display + $c_width] [expr $y + $c_height] -width 0 -fill $gui_options(p_matched) -tag [list $box_name zoom p_matched]
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: [lindex $i 1]  U(n): [lindex $i 2]\""
                $win.frame.canvas bind $box_name <ButtonPress> "p_history_whynot_view $win 1 [lindex $i 1] [lindex $i 2] [lindex $names $index] [lindex $x 0]"
              }
              2 {
                $win.frame.canvas create rectangle $x_display $y [expr $x_display + $c_width] [expr $y + $c_height] -width 0 -fill $gui_options(p_mismatched) -tag [list $box_name zoom p_mismatched]
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar {Whynot: [lindex $reasons [lindex $i 1]]}"
                $win.frame.canvas bind $box_name <ButtonPress> "p_history_whynot_view $win 2 {[lindex $reasons [lindex $i 1]]} {} [lindex $names $index] [lindex $x 0]"
              }
            }

            $win.frame.canvas bind $box_name <Leave>  "set $win.notesvar \"\""
 
            incr y $c_height
            incr index
          }

          incr x_display $c_width
        }
      }
    }
  }

  for {set x 0} {$x < $t_width} {incr x $c_width} {
     $win.frame.canvas create line $x 0 $x $t_height -width 1 -f black -tag [list grid grid_vert zoom]
  }

  for {set y $c_height} {$y < $t_height} {incr y $c_height} {
     $win.frame.canvas create line 0 $y $t_width $y -width 1 -f black -tag [list grid zoom]
  }

  set scale 1.0
  set grid black
  set g_c_height $c_height
  set g_c_width $c_width
  
  p_button_toggle $win normal
}


proc load_p_history_data {win model} {

  global top_dir

  set fname [tk_getOpenFile -title "Load Production History" -initialdir $top_dir] 

  if {$fname != ""} {
    draw_p_trace $win $model $fname
  }
}


proc p_trace_zoom_out {win} {

   global $win.scale
   upvar $win.scale scale

   set scale [expr .5 * $scale]
 
   $win.frame.canvas scale zoom 0 0 0.5 1.0
   $win.frame.canvas configure -width [expr .5 * [$win.frame.canvas cget -width]]
   $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
}

proc p_trace_zoom_in {win} {

  global $win.scale
  upvar $win.scale scale

  if {$scale < 16} {
    set scale [expr 2 * $scale]

    $win.frame.canvas scale zoom 0 0 2.0 1.0
    $win.frame.canvas configure -width [expr 2.0 * [$win.frame.canvas cget -width]]
    $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
  }
}


proc p_trace_grid {win} {

  global $win.grid_state
  upvar $win.grid_state grid

  if {$grid == ""} {
    $win.frame.canvas itemconfigure grid -f black
    set grid black
  } elseif {$grid == "vert"} {
    $win.frame.canvas itemconfigure grid -f ""
    set grid ""
  } else {
    $win.frame.canvas itemconfigure grid_vert -f ""
    set grid vert
  }
}

proc p_history_p_view {win name text} {

  # make a new window 
  set top [toplevel [new_variable_name ".p_history_p_view"]]

  wm geometry $top [get_configuration .p_history_p_view $top]

  wm title $top "[wm title $win] Production: $name"     

  frame $top.frame -borderwidth 0  
    
  set text_box [text $top.frame.text -font text_font \
                                     -yscrollcommand "$top.frame.scrl set" ]

  $text_box insert 0.0 $text

  $text_box configure -state disabled

  set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
  place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
  pack $scrl_bar -side right -fill y 
  pack $text_box -side left -expand 1 -fill both
 
}

proc p_history_whynot_view {win which t1 t2 name time} {

  # make a new window 
  set top [toplevel [new_variable_name ".p_history_whynot_view"]]

  wm geometry $top [get_configuration .p_history_whynot_view $top]

  wm title $top "[wm title $win] Whynot Production: $name at time $time"     

  frame $top.frame -borderwidth 0  
    
  set text_box [text $top.frame.text -font text_font \
                                     -yscrollcommand "$top.frame.scrl set" ]

  

  switch $which {
    0 {
      $text_box insert 0.0 "Production $name Was selected at time $time\nUtility: $t1 U(n): $t2"
    }
    1 {
      $text_box insert 0.0 "Production $name matched but was not selected at time $time\nUtility: $t1 U(n): $t2"
    }
    2 {
      $text_box insert 0.0 "Production $name did not match at time $time\nWhynot: $t1"
    }
  }

  $text_box configure -state disabled

  set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
  place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
  pack $scrl_bar -side right -fill y 
  pack $text_box -side left -expand 1 -fill both
 
}




proc save_p_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save Production History" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "production-history"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save Production History warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global p_history_warnings

      set p_history_warnings ""
            
      set warning_monitor [add_new_cmd warning_monitor "p_record_warnings" "Environment command for capturing warnings during Save Production History."]

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "production-history" $fname "Data saved from Production History window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save Production History error" \
                      -message "Get-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save Production History problem" \
                      -message "Get-history-data returned failure result.\n$p_history_warnings."
      }
    }
  }
}



proc save_phistory_trace {win} {
  set fname [tk_getSaveFile -title "Save production history as" -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  global $win.label_offset
  upvar $win.label_offset label_offset

  if {$fname != ""} {
    $win.frame.canvas postscript -file $fname -width [expr -$label_offset + [$win.frame.canvas cget -width]] -height [$win.frame.canvas cget -height] -x $label_offset -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.canvas cget -height] -pagewidth [expr -$label_offset + [$win.frame.canvas cget -width]]
  }
}

proc save_phistory_trace_multi {win} {

  global $win.label_offset
  upvar $win.label_offset label_offset

  save_multi_page_image $win "Save production history as" $label_offset
}

proc save_multi_page_image {win prompt {x_offset 0}} {
  set fname [tk_getSaveFile -title $prompt -filetypes {{"PostScript" "*.ps"}}]

  if {$fname != ""} {  
 
   set width 1400.0
   set height 400

   set xMax [expr -$x_offset + [$win.frame.canvas cget -width]]
   set NOP [expr ceil ($xMax / $width)]   
  
# The following code was modified from code written by Robert Heller
# in a file called bridge.tcl which was posted to comp.lang.tcl as
# an example of producing multi-page ps files.

   set prFile [open $fname w]

  puts $prFile "%!PS-Adobe-2.0"
  puts $prFile "%%Creator: ACT-R Environment Copyright 2011 Dan Bothell"
  puts $prFile "%%Title: Production History Chart"
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

      set eps "[$win.frame.canvas postscript -height $height -width $width -x [expr $xoff + $x_offset] -y 0 -pageanchor nw -pagex 2.0i -pagey 0.5i -pagewidth 10.0i -rotate 1]"

 
      set EOC [string first "%%BeginProlog\n" "$eps"]
      set EOF [expr [string first "%%EOF\n" "$eps"] - 1]

      puts $prFile "[StripPSComments [string range $eps $EOC $EOF]]"
      puts $prFile "EndInclude showpage"
      incr pageNo
  }
  puts $prFile "%%EOF"
  close $prFile
  }
}

proc StripPSComments {PSString} {
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


add_p_history_type "grid" select_ptrace
