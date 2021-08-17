proc pick_buffers {} {

  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Select Buffers" -message "Selecting Buffers to record requires a current model."
  } else {

    set win .pick_buffers_$model

    if {[winfo exists $win] == 1} {
      wm deiconify $win
      raise $win
    } else {

      toplevel $win

      wm withdraw $win

      wm geometry $win [get_configuration .pick_buffers $win]

      record_new_window $win $win $model

      set buffers [lindex [call_act_r_command "buffers" $model ""] 0]

      set y 0

      foreach b [lsort $buffers] {
    
        set name [string tolower $b]

        checkbutton $win.$name -text $b \
                    -font checkbox_font \
                    -command "toggle_buffer $win $name $model" \
                    -onvalue 1 -offvalue 0 \
                    -variable $win.$name \
                    -height 24 -anchor nw

        bind $win.$name <Destroy> "global $win.$name
                                   unset $win.$name
                                  "

        place $win.$name -x 10 -y $y -height 25 -relwidth 1.0 -width -10
        incr y 25
      }

      set used [lindex [call_act_r_command "used-production-buffers" $model ""] 0]

      foreach b $used {
    
        set name [string tolower $b]

        upvar #0 $win.$name v

        if {$v == 0} {
          $win.$name invoke
        }
      }

      # don't destroy it when the close button pressed
      # but do stop recording because that is just 
      # enabled to avoid the warnings 

      wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"



      wm deiconify $win
    }
  }
}

proc toggle_buffer {win name model} {
  
  upvar #0 $win.$name v

  if {$v == 1} {
    call_act_r_command "record-history" $model [list $name]
  } else {
    call_act_r_command "stop-recording-history" $model [list $name]
  }
}


button [control_panel_name].pick_buffers -command pick_buffers -text "Select Buffers" -font button_font

# put that button on the control panel

# Don't put the button there -- let the other tools open it so that buffer-history is recorded first

# pack [control_panel_name].pick_buffers
