
proc make_parameter_viewer {} {
  
  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Parameter Viewer" -message "The parameter viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .param_viewer]]
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .param_viewer $win]
  
    # frame and list box for modules

    set list_frame_1 [frame $win.list_frame_1 -borderwidth 2]  
  
    set list_box_1 [listbox $list_frame_1.list_box -listvar \
                            $list_frame_1.list_box.var \
                            -yscrollcommand "$list_frame_1.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

    set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl -command "$list_box_1 yview"]

    global $list_box_1.var

    set $list_box_1.var [lindex [call_act_r_command "modules-with-parameters" $model] 0]

    # Frame and list box for parameters

    set list_frame_2 [frame $win.list_frame -borderwidth 2]  
  
    set list_box_2 [listbox $list_frame_2.list_box -listvar \
                            $list_frame_2.list_box.var \
                            -yscrollcommand "$list_frame_2.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

    set list_scroll_bar_2 [scrollbar $list_frame_2.list_scrl -command "$list_box_2 yview"]


    # The lables for the sections

    set l1 [label $win.l1 -text "Modules" -justify left -font label_font]
    set l2 [label $win.l2 -text "Parameters" -justify left -font label_font]

    # frame for the parameter display

    set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
    set text_box_1 [text $text_frame_1.text -xscrollcommand \
                         "$text_frame_1.text_scrl set"  \
                         -font text_font -width 200 -wrap none]

    set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 xview" -orient horizontal]


    # make selection lists call the appropriate updater 

    bind $list_box_1 <<ListboxSelect>> "select_param_module $list_box_1 $list_box_2 $text_box_1 $model"
    bind $list_box_2 <<ListboxSelect>> "select_param_value  $list_box_2 $text_box_1 $model"

    pack $list_scroll_bar_1 -side right -fill y 
    pack $list_box_1 -side left -expand 1 -fill both
    pack $text_scroll_bar_1 -side bottom -fill x
    pack $text_box_1 -side left -expand 1 -fill both

    pack $list_scroll_bar_2 -side right -fill y 
    pack $list_box_2 -side left -expand 1 -fill both

    place $l1 -relx 0.0 -y 0 -height 25 -relwidth .5
    place $list_frame_1 -relx 0.0 -y 25 -relheight 1.0 -height -75 -relwidth .5

    place $l2 -relx .5 -y 0 -height 25 -relwidth .5
    place $list_frame_2 -relx .5 -y 25 -relheight 1.0 -height -75 -relwidth .5

    place $text_frame_1 -relx 0.0 -rely 1.0 -y -45 -height 45 -relwidth 1.0
  
    set_update_script $win "select_param_value $list_box_2 $text_box_1 $model"

    wm deiconify $win
    focus $win
  }
}

proc select_param_module {modulewin paramswin text model} {
 
  set selection [$modulewin curselection]

  global $paramswin.var
  $paramswin selection clear 0 end
  update_text_pane $text ""

  if {$selection != ""} {
     set $paramswin.var [lindex [call_act_r_command "modules-parameters" $model [list [$modulewin get $selection]]] 0]
  } else {
    set $paramswin.var ""
  }
}

proc select_param_value {paramswin target_win model} {

  set selection [$paramswin curselection]

  if {$selection != ""} {
    update_text_pane $target_win [lindex [call_act_r_command "printed-parameter-details" $model [list [$paramswin get $selection]]] 0]
  } else {
    update_text_pane $target_win ""
  }
}
  

button [control_panel_name].param_viewer -command {make_parameter_viewer} -text "Parameters" -font button_font
pack [control_panel_name].param_viewer
