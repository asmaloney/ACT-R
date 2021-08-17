proc make_buffer_status_viewer {} {

  set model [currently_selected_model]
  
  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Buffer Status" -message "Inspector tools require a current model."
  } else {
    set win [toplevel [new_variable_name .bufferstatus]]
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .bufferstatus $win]
  
    set list_frame [frame $win.list_frame -borderwidth 2]  
  
    set list_box [listbox $list_frame.list_box \
                          -listvar $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode single \
                          -exportselection 0 -font list_font -bd 0]

    set list_scroll_bar [scrollbar $list_frame.list_scrl -command "$list_box yview"]

    set text_frame [frame $win.text_frame -borderwidth 0]  
   
    set text_box [text $text_frame.text -yscrollcommand \
                       "$text_frame.text_scrl set" -state disabled \
                       -font text_font]
  
    bind $list_box <<ListboxSelect>> "select_buffer_status $list_box $text_box $model"

    set text_scroll_bar [scrollbar $text_frame.text_scrl -command "$text_box yview"]

    place $list_frame -relx 0.0 -rely 0.0 -relheight 1.0 -relwidth .4
    place $text_frame -relx .4 -rely 0.0 -relheight 1.0 -relwidth .6
     
    pack $list_scroll_bar -side right -fill y 
    pack $list_box -side left -expand 1 -fill both
    pack $text_scroll_bar -side right -fill y
    pack $text_box -side left -expand 1 -fill both

    set_update_script $win "update_buffer_list $list_box $text_box $model"

    wm deiconify $win
    focus $win

  }
}

proc update_buffer_list {list text_box model} {

  set buffers [lindex [call_act_r_command "buffers" $model [list 1]] 0]

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

  select_buffer_status $list $text_box $model
}

proc select_buffer_status {list_box text_box model} {

  if {[$list_box curselection] != ""} {
    update_text_pane $text_box [lindex [call_act_r_command "printed-buffer-status" $model [list [$list_box get [$list_box curselection]]]] 0]
  } else {
    update_text_pane $text_box ""
  }
}


# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].buffer_status -command {make_buffer_status_viewer} \
                    -text "Buffer Status" -font button_font

# put that button on the control panel

pack [control_panel_name].buffer_status
