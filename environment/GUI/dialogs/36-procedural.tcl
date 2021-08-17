proc make_procedural_viewer {} {

  set model [currently_selected_model]
  
  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Procedural" -message "Procedural viewer requires a current model."
  } else {
 
    set win [toplevel [new_variable_name .procedural]]
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .procedural $win]
  
    set list_frame [frame $win.list_frame -borderwidth 2]  
  
    set list_box [listbox $list_frame.list_box \
                          -listvar $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode single -font list_font\
                          -exportselection 0 -bd 0]

    set list_scroll_bar [scrollbar $list_frame.list_scrl \
                                   -command "$list_box yview"]

    set text_frame [frame $win.text_frame -borderwidth 0]  

    set text_box [text $text_frame.text -yscrollcommand \
                       "$text_frame.text_scrl set" -state disabled \
                       -font text_font]

    set text_scroll_bar [scrollbar $text_frame.text_scrl -command "$text_box yview"]

    bind $list_box <<ListboxSelect>> "select_production $list_box $text_box $model"

    set why_not [button $win.whynot_button -text "Why not?" -font button_font \
                            -command "show_why_not $list_box $model"]

  
 
    place $why_not -x 2 -y 5 -width 75 -height 24

    place $list_frame -relx 0.0 -y 30 -relheight 1.0 -height -30 -relwidth .4
    place $text_frame -relx .4 -y 30 -relheight 1.0 -height -30 -relwidth .6
     
    pack $list_scroll_bar -side right -fill y 
    pack $list_box -side left -expand 1 -fill both
    pack $text_scroll_bar -side right -fill y
    pack $text_box -side left -expand 1 -fill both

    set_update_script $win "update_production_list $list_box $text_box $model"

    wm deiconify $win
    focus $win
  }
}

proc update_production_list {list text_box model} {

  set productions [lindex [call_act_r_command "all-productions" $model] 0]

  global options_array

  if {$options_array(sort_lists) == 1} {
    set productions [lsort -dictionary $productions]
  }

  global $list.var

  set selection [$list curselection]

  if {$selection != ""} {
    set name [$list get $selection]
  }

  set $list.var $productions

  if {$selection != ""} { 

    $list selection clear 0 end
    set newpos [lsearch -exact $productions $name]

    if {$newpos != -1} {
      $list selection set $newpos
    }
  }

  select_production $list $text_box $model
}


proc select_production {list_box text_box model} {
  if {[$list_box curselection] != ""} {
    update_text_pane $text_box [lindex [call_act_r_command "production-details" $model [list [$list_box get [$list_box curselection]]]] 0]
  } else {
    update_text_pane $text_box ""
  }
}

proc show_why_not {prods model} {
  # get the current selection - knowing it's a single selection list box
  set selection [$prods curselection]

  if {$selection != "" } {
      
    set prod [$prods get $selection]
    
    # make a new window 
    set top [toplevel [new_variable_name ".whynot"]]

    wm geometry $top [get_configuration .whynot $top]

    record_new_window $top "Whynot $prod" $model
    
    # create a text_box to hold the output and put it in the window
    # also need a frame to hold it and the scroll bar

    frame $top.frame -borderwidth 0  
    
    set text_box [text $top.frame.text -font text_font -state disabled \
                                       -yscrollcommand "$top.frame.scrl set" ]

    set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
    place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
    pack $scrl_bar -side right -fill y 
    pack $text_box -side left -expand 1 -fill both
 
    update_text_pane $text_box [lindex [call_act_r_command "whynot-text" $model [list $prod]] 0]
  }
}


# Make a button for the control panel that will open a new procedural viewer

button [control_panel_name].procedural -command {make_procedural_viewer} -text "Procedural" -font button_font

# put that button on the control panel

pack [control_panel_name].procedural
