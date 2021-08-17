proc make_buffer_viewer {} {

  set model [currently_selected_model]
  
  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Buffer Viewer" -message "Inspector tools require a current model."
  } else {
    set win [toplevel [new_variable_name .buffers]]
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .buffers $win]
  
    set list_frame [frame $win.list_frame -borderwidth 2]  
  
    set list_box [listbox $list_frame.list_box \
                          -listvar $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode single \
                          -exportselection 0 -font list_font -bd 0]

    set list_scroll_bar [scrollbar $list_frame.list_scrl -command "$list_box yview"]

    set other_frame [frame $win.other_frame -borderwidth 0]
    set text_frame [frame $other_frame.text_frame -borderwidth 0]  

    set b1 [button $other_frame.contents -text "Contents" -font button_font -foreground black -relief raised] 
    set b2 [button $other_frame.status -text "Status" -font button_font -foreground gray -relief sunken] 
  
    set var [new_variable_name buffer_state]

    upvar #0 $var s
    set s "contents"

    set text_box [text $text_frame.text -yscrollcommand \
                       "$text_frame.text_scrl set" -state disabled \
                       -font text_font]
  
    $b1 configure -command "switch_buffer_view $b1 $b2 $var contents $list_box $text_box $model"
    $b2 configure -command "switch_buffer_view $b2 $b1 $var status $list_box $text_box $model"

    global tcl_platform
    if {$tcl_platform(os) == "Darwin"} {
      $b1 configure -state active
    }

    bind $list_box <<ListboxSelect>> "select_buffer_viewer $list_box $text_box $model $var"

    set text_scroll_bar [scrollbar $text_frame.text_scrl -command "$text_box yview"]

    place $list_frame -relx 0.0 -rely 0.0 -relheight 1.0 -relwidth .4
    place $other_frame -relx .4 -rely 0.0 -relheight 1.0 -relwidth .6
     
    pack $list_scroll_bar -side right -fill y 
    pack $list_box -side left -expand 1 -fill both
    place $b1 -x 0 -y 0 -height 25 -relwidth .5
    place $b2 -relx .5 -y 0 -height 25 -relwidth .5
    place $text_frame -x 0 -y 25 -relheight 1.0 -height -25 -relwidth 1.0
    pack $text_scroll_bar -side right -fill y
    pack $text_box -side left -expand 1 -fill both

    set_update_script $win "update_buffers_list $list_box $text_box $model $var"

    wm deiconify $win
    focus $win

  }
}


proc switch_buffer_view {b1 b2 var state lb tb m} {

  global tcl_platform

  if {$tcl_platform(os) == "Darwin"} {
    $b1 configure -state active
    $b2 configure -state normal
  } else {
    $b1 configure -relief raised -foreground black
    $b2 configure -relief sunken -foreground gray
  }

  upvar #0 $var x
  set x $state

  select_buffer_viewer $lb $tb $m $var
}

proc update_buffers_list {list text_box model var} {

  set buffers [lindex [call_act_r_command "buffers" $model [list 1]] 0]

  global options_array

  if {$options_array(sort_lists) == 1} {
    set buffers [lsort -dictionary $buffers]
  }

  global $list.var

  set selection [$list curselection]

  if {$selection != ""} {
    set name [$list get $selection]
  }

  set $list.var $buffers

  if {$selection != ""} { 
  # check if the selection is still available

    $list selection clear 0 end
    set newpos [lsearch -exact $buffers $name]

    if {$newpos != -1} {
      $list selection set $newpos
    }
  }

  select_buffer_viewer $list $text_box $model $var
}

proc select_buffer_viewer {list_box text_box model var} {

  upvar #0 $var mode

  if {[$list_box curselection] != ""} {

    if {$mode == "contents"} {

      if {[lindex [call_act_r_command "buffer-read" $model [list [$list_box get [$list_box curselection]]]] 0] == ""} {
        if {[lindex [call_act_r_command "multi-buffer-p" $model [list [$list_box get [$list_box curselection]]]] 0] == ""} {
          update_text_pane $text_box "Buffer is empty"
        } else {
          set text "Empty multi-buffer\nPossible chunks are:\n"
          foreach c [call_act_r_command "get-m-buffer-chunks" $model [list [$list_box get [$list_box curselection]]]] {
            set text "$text[lindex [call_act_r_command "printed-chunk" $model [list $c]] 0]\n"
          }
          update_text_pane $text_box $text
        }
      } else {
        update_text_pane $text_box [lindex [call_act_r_command "printed-buffer-chunk" $model [list [$list_box get [$list_box curselection]]]] 0]
      }
    } else {
      update_text_pane $text_box [lindex [call_act_r_command "printed-buffer-status" $model [list [$list_box get [$list_box curselection]]]] 0]
    }
  } else {
    update_text_pane $text_box ""
  }
}


# Make a button for the control panel that will open a new buffer viewer

button [control_panel_name].buffers -command {make_buffer_viewer} -text "Buffers" -font button_font

# put that button on the control panel

pack [control_panel_name].buffers
