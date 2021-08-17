proc select_audicon {} {

  set model [currently_selected_model]

  if {$model == "nil"} {

    tk_messageBox -icon info -type ok -title "Audicon" -message "Inspector tools require a current model."
  } else {

    set win ".audicon_$model"

    if {[winfo exists $win] == 1} {
      wm deiconify $win
      raise $win
    } else {

      toplevel $win
      wm withdraw $win

      record_new_window $win "Audicon" $model

      wm geometry $win [get_configuration .audicon $win]

      set f [frame $win.frame -borderwidth 0]  
    
      set t [text $f.text -font text_font -yscrollcommand "$f.scrl set" -state disabled]
          
      set s [scrollbar $f.scrl -command "$t yview"]

      pack $s -side right -fill y 
      pack $t -side left -expand 1 -fill both
  
      place $f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

      set_update_script $win "update_audicon_view $t $model"

      wm deiconify $win
      focus $win
    }
  }
}


proc update_audicon_view {text model} {
  update_text_pane $text [lindex [call_act_r_command "printed-audicon" $model] 0]
}


button [control_panel_name].audicon_button \
       -command {select_audicon} -text "Audicon" -font button_font

pack [control_panel_name].audicon_button

