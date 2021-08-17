proc select_event_queue {} {

  if {[winfo exists .event_queue] == 1} {
    wm deiconify .event_queue
    raise .event_queue
  } else {
    # make it now
    toplevel .event_queue

    wm withdraw .event_queue

    wm geometry .event_queue [get_configuration .event_queue]

    set f [frame .event_queue.frame -borderwidth 0]  
    
    set t [text $f.text -font text_font -yscrollcommand "$f.scrl set" -state disabled]
          
    set s [scrollbar $f.scrl -command "$t yview"]

#    set new_tags [concat [list [new_variable_name "focus_tag"]] [bindtags .event_queue]]

#    bindtags .event_queue $new_tags

#    bind [lindex $new_tags 0] <FocusIn> {
#      catch {update_text_pane .event_queue.frame.text [lindex [call_act_r_command "mp-queue-text" nil 1] 0]}
#    }    

    set_update_script .event_queue {update_text_pane .event_queue.frame.text [lindex [call_act_r_command "mp-queue-text" nil 1] 0]}

    pack $s -side right -fill y 
    pack $t -side left -expand 1 -fill both
  
    place $f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

    # now show the window 

    wm deiconify .event_queue
    focus .event_queue
  }
}


button [control_panel_name].event_queue_button \
       -command {select_event_queue} -text "Event Queue" -font button_font

pack [control_panel_name].event_queue_button

