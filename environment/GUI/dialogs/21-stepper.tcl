global stop_type
global stepper_tutor
global tutor_ans
global tutor_bindings
global tutor_answers
global step_now
global stepper_tutorable
global stepper_tutored

global current_stepper_id

global tutor_selections

frame [control_panel_name].step_frame -borderwidth 0 

checkbutton [control_panel_name].step_frame.step_all_events -text "Step All" -font checkbox_font \
                -variable step_for_all -command {change_step_all} 

set current_stepper_id -1
set step_for_all 0
set revert_step_all 0

proc stepper_pause {} {
  global step_for_all
  global revert_step_all
  global current_stepper_id

  if {$step_for_all == 0} {
    set step_for_all 1
    call_act_r_command "set-stepper-step-all" nil [list $current_stepper_id true]
    set revert_step_all 1
  }

  select_stepper
}


proc change_step_all {} {
  global step_for_all
  global current_stepper_id

  if {$current_stepper_id != -1} {

    if {$step_for_all == 0} {
      set step false
    } else {
      set step true
    }

    call_act_r_command "set-stepper-step-all" nil [list $current_stepper_id $step]
  }
}

proc stepper_button_state_control {state} {
 .stepper.step configure -state $state
 .stepper.stop configure -state $state
 .stepper.run_until configure -state $state
} 



proc select_stepper {} {
  global current_stepper_id
  global step_for_all
  global stepper_wait
  global stepper_stepped
  global close_stepper
  global reset_stepper

  if {[winfo exists .stepper] == 1} {
    wm deiconify .stepper
    raise .stepper
  } else {

    if {$step_for_all == 0} {
      set s null
    } else {
      set s true
    }

    set start [call_act_r_command "start-stepper" nil [list $s false $stepper_wait $stepper_stepped]]

    if {[lindex $start 0]  != "true" } {
      tk_messageBox -icon info -type ok -title "Stepper" \
                    -message [lindex $start 1]
      return 0
    }

    set current_stepper_id [lindex $start 1]

    toplevel .stepper
    wm withdraw .stepper
    wm title .stepper "Stepper"

    wm geometry .stepper [get_configuration .stepper]

    tk_optionMenu .stepper.run_until_type stop_type Time Production Module

    .stepper.run_until_type configure -font button_font

    [.stepper.run_until_type cget -menu] configure -font menu_font

    button .stepper.run_until -text "Run Until:" -font button_font -command {stepper_run_until}                  

    entry .stepper.run_until_time -width 6 -font text_font -textvariable run_until_time 

    label .stepper.current -text "Last Stepped:" -justify left -font label_font
    label .stepper.next -text "Next Step:" -justify left -font label_font

    button .stepper.step -text "Step" -font button_font -command {stepper_step_button}
    button .stepper.stop -text "Stop" -font button_font -command {stepper_stop_button}
    
    label .stepper.current_text -font text_font \
          -textvar .stepper.current_text.var -justify left -anchor nw

    label .stepper.next_text -font text_font \
          -textvar .stepper.next_text.var -justify left -anchor nw

    frame .stepper.prod_frame -borderwidth 0
    frame .stepper.prod_frame.f4 -borderwidth 0  
  
    label .stepper.prod_frame.f4.list_title -textvar .stepper.prod_frame.f4.list_title.var \
           -anchor nw -justify left -font label_font

    set .stepper.prod_frame.f4.list_title.var ""

    frame .stepper.prod_frame.f4.f -borderwidth 0  

    listbox .stepper.prod_frame.f4.f.list -listvar .stepper.prod_frame.f4.f.list.var \
            -yscrollcommand ".stepper.prod_frame.f4.f.scrl set" \
            -selectmode single -exportselection 0 -font list_font -bd 0
   
    bind .stepper.prod_frame.f4.f.list <<ListboxSelect>> {
      update_instantiation_viewers %W
    }

    scrollbar .stepper.prod_frame.f4.f.scrl -command ".stepper.prod_frame.f4.f.list yview"


    frame .stepper.prod_frame.f3 -borderwidth 0
    frame .stepper.prod_frame.f3.f -borderwidth 0

    label .stepper.prod_frame.f3.production \
          -textvariable .stepper.prod_frame.f3.production.var  \
          -justify left -font label_font

    text .stepper.prod_frame.f3.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f3.f.scrl set" \
         -state disabled 

    
    scrollbar .stepper.prod_frame.f3.f.scrl -command ".stepper.prod_frame.f3.f.text yview"

    frame .stepper.prod_frame.f2 -borderwidth 0
    frame .stepper.prod_frame.f2.f -borderwidth 0

    label .stepper.prod_frame.f2.bindings \
          -textvariable .stepper.prod_frame.f2.bindings.var \
          -justify left -font label_font

    text .stepper.prod_frame.f2.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f2.f.scrl set" \
         -state disabled
  
    scrollbar .stepper.prod_frame.f2.f.scrl -command ".stepper.prod_frame.f2.f.text yview"


    checkbutton .stepper.stepper_tutor -text "Tutor Mode" -font checkbox_font \
                -variable stepper_tutor -command {select_tutor_mode} 

    .stepper.stepper_tutor deselect


    frame .stepper.prod_frame.f5 -borderwidth 0
    frame .stepper.prod_frame.f5.f -borderwidth 0

    label .stepper.prod_frame.f5.production \
          -textvariable .stepper.prod_frame.f5.production.var  \
          -justify left -font label_font

    text .stepper.prod_frame.f5.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f5.f.scrl set" \
         -state disabled 

    scrollbar .stepper.prod_frame.f5.f.scrl -command ".stepper.prod_frame.f5.f.text yview"

    place .stepper.step -x 2 -y 2 -width 50 -height 25
    place .stepper.stop -x 55 -y 2 -width 50 -height 25
 
    place .stepper.run_until -x 108 -y 2 -width 80 -height 25
    place .stepper.run_until_type -x 190 -y 2 -width 100 -height 25
    place .stepper.run_until_time -x 292 -y 2 -width -394 -relwidth 1.0 -height 25
    place .stepper.stepper_tutor -x -100 -relx 1.0 -y 5 -height 25 -width 100

    place .stepper.next -x 0 -y 30 -height 25 -width 90 
    place .stepper.next_text -x 102 -y 32 -relwidth 1.0 -width -92 -height 23 

    place .stepper.current -x 0 -y 60 -height 25 -width 90 
    place .stepper.current_text -x 102 -y 62 -relwidth 1.0 -width -92 -height 23 

    place .stepper.prod_frame -x 0 -y 90 -relwidth 1.0 -relheight 1.0 -height -90

    place .stepper.prod_frame.f5 -relx 0.0 -rely .5 -relwidth .4 -relheight .5

    pack .stepper.prod_frame.f5.production -side top 

    pack .stepper.prod_frame.f5.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f5.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f5.f -side top -expand 1 -fill both

    place .stepper.prod_frame.f4 -relx 0.0 -rely 0.0 -relwidth .4 -relheight .5

    pack .stepper.prod_frame.f4.list_title -side top 

    pack .stepper.prod_frame.f4.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f4.f.list -side left -expand 1 -fill both

    pack .stepper.prod_frame.f4.f -side top -expand 1 -fill both

    place .stepper.prod_frame.f3 -relx 0.4 -rely 0.4 -relwidth .6 -relheight .6

    pack .stepper.prod_frame.f3.production -side top 

    pack .stepper.prod_frame.f3.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f3.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f3.f -side top -expand 1 -fill both

    place .stepper.prod_frame.f2 -relx 0.4 -rely 0.0 -relwidth .6 -relheight .4

    pack .stepper.prod_frame.f2.bindings -side top 

    pack .stepper.prod_frame.f2.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f2.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f2.f -side top -expand 1 -fill both


    send_cmd "monitor" [list "clear-all-start" $close_stepper]
    send_cmd "monitor" [list "reset-step1" $reset_stepper]

    bind .stepper.step <Destroy> {

      send_cmd "remove-monitor" [list "clear-all-start" $close_stepper]
      send_cmd "remove-monitor" [list "reset-step1" $reset_stepper]
      
      call_act_r_command "stop-stepper" nil [list $current_stepper_id]

      global step_for_all
      global revert_step_all

      if {$revert_step_all == 1} {
        set step_for_all 0
        set revert_step_all 0
      }

      set current_stepper_id -1
    }

    reset_stepper_tool ""

    wm deiconify .stepper

    call_act_r_command "set-stepper-ready" nil [list $current_stepper_id]
    
  }
}

proc close_stepper_tool {model} {
  destroy .stepper
}

proc reset_stepper_tool {model} {
  global .stepper.current_text.var
  global .stepper.prod_frame.f4.list_title.var
  global .stepper.prod_frame.f3.production.var
  global .stepper.prod_frame.f2.bindings.var
  global .stepper.prod_frame.f5.production.var 
  global .stepper.next_text.var
  global .stepper.prod_frame.f4.f.list.var
  global stepper_tutor
  global stepper_tutored
  global stepper_tutorable
  global current_stepper_id

  if {[winfo exists .tutor_response] == 1} {

    global tutor_ans
    set tutor_ans ""
    destroy .tutor_response
  }

  set .stepper.next_text.var ""
  set .stepper.current_text.var ""
 
  set .stepper.prod_frame.f4.list_title.var ""
  set .stepper.prod_frame.f5.production.var ""
  set .stepper.prod_frame.f2.bindings.var ""
  set .stepper.prod_frame.f3.production.var ""

  update_text_pane .stepper.prod_frame.f5.f.text ""
  update_text_pane .stepper.prod_frame.f2.f.text ""
  update_text_pane .stepper.prod_frame.f3.f.text ""

  set .stepper.prod_frame.f4.f.list.var ""

  set stepper_tutor 0
  set stepper_tutored 0
  set stepper_tutorable 0

  stepper_button_state_control disabled

  call_act_r_command "set-stepper-tutoring" nil [list $current_stepper_id false]

}


proc select_tutor_mode {} {
  global tutor_bindings 
  global tutor_answers
  global stepper_tutored
  global stepper_tutor
  global stepper_tutorable
  global current_stepper_id

  if {[winfo exists .tutor_response] == 1} {

    global tutor_ans
    set tutor_ans ""
    destroy .tutor_response
  }

  if {[winfo exists .tutor_cr] == 1} {
    destroy .tutor_cr
  }

  if {[winfo exists .tutor_r] == 1} {
    destroy .tutor_r
  }

  if [array exists tutor_bindings] {
    array unset tutor_bindings 
  }  
  if [array exists tutor_answers] {
    array unset tutor_answers
  }  

  if {$stepper_tutored && !$stepper_tutor} {
    set stepper_tutored 0
  }  elseif {$stepper_tutorable && $stepper_tutor} { 
    set stepper_tutored 1
  }

  if {$stepper_tutor} {
    call_act_r_command "set-stepper-tutoring" nil [list $current_stepper_id true]
  } else {
    call_act_r_command "set-stepper-tutoring" nil [list $current_stepper_id false]
  }

  update_instantiation_viewers .stepper.prod_frame.f4.f.list
}


proc wait_for_stepper {model event} {

  global .stepper.next_text.var

  set .stepper.next_text.var $event

  stepper_button_state_control normal

}

proc display_stepper_stepped {model text items tutorable p1 p2 p3 p4} {
  global .stepper.current_text.var
  global .stepper.prod_frame.f4.list_title.var
  global .stepper.prod_frame.f3.production.var
  global .stepper.prod_frame.f2.bindings.var
  global .stepper.prod_frame.f5.production.var 
  global stepper_tutored
  global stepper_tutor
  global .stepper.next_text.var
  global stepper_tutorable
  global options_array
  global tutor_bindings

  set .stepper.next_text.var ""

  set .stepper.current_text.var $text

  set .stepper.prod_frame.f4.list_title.var $p1
  set .stepper.prod_frame.f5.production.var $p2
  set .stepper.prod_frame.f2.bindings.var $p3
  set .stepper.prod_frame.f3.production.var $p4

  update_text_pane .stepper.prod_frame.f5.f.text ""
  update_text_pane .stepper.prod_frame.f2.f.text ""
  update_text_pane .stepper.prod_frame.f3.f.text ""

# quick hack to fix an issue with my JSON parser not converting null to ""

  if {$items == "null"} {set items ""}

  if {$tutorable =="null"} {
    set stepper_tutorable 0
  } else {
    set stepper_tutorable 1
  }
  
  if {$tutorable != "null" && $stepper_tutor} {

    if {[string compare -nocase $tutorable "conflict-resolution"] == 0} {
      set stepper_tutored 2
    } elseif {[string compare -nocase $tutorable "start-retrieval"] == 0} {
      set stepper_tutored 3
    } else {
      set stepper_tutored 1
    }
    
    if {[llength $items] > 1} {
      set items [list [lindex $items 0]]
    }
  } else {
    set stepper_tutored 0
  }
  update_list_box .stepper.prod_frame.f4.f.list $items 1 1

  if {$stepper_tutored == 2 } {
    
    set data [call_act_r_command "update-stepper" nil false]
    
    set tutor_bindings(productions) [lindex $data 0]

    display_conflict_resolution
  } 

  if {$stepper_tutored == 3 } {
    
    set data [call_act_r_command "update-stepper" nil false]
    
    set tutor_bindings(retrieval) [lindex $data 0]

    display_retrieval_choice
  }


  if $options_array(update_when_stepped) {
    update_registered_windows
  }
}


proc stepper_step_button {} {
  global stepper_tutor
  global tutor_bindings
  global current_stepper_id
  global stepper_tutored

  if {!$stepper_tutor || [array names tutor_bindings] == ""} { 

    stepper_button_state_control disabled
    set result [call_act_r_command "step-stepper" nil [list $current_stepper_id]]

    if {[lindex $result 0] == "null"} {
      tk_messageBox -icon info -type ok -title "Step error" \
                    -message [lindex $result 1]

      stepper_button_state_control normal
    }
     

  } else {
    if {$stepper_tutored == 1} {
      tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "You must complete the production instantiation before continuing in tutor mode."
    } elseif {$stepper_tutored == 2} {
      tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "You must complete the conflict-resolution selection before continuing in tutor mode."
    } elseif {$stepper_tutored == 3} {
      tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "You must complete the retrieved chunk selection before continuing in tutor mode."
    }

  }
}    


proc stepper_stop_button {} { # always let the user stop even in tutor mode now

  global current_stepper_id

  stepper_button_state_control disabled
  set result [call_act_r_command "step-stepper" nil [list $current_stepper_id true]]

  if {[lindex $result 0] == "null"} {
    tk_messageBox -icon info -type ok -title "Stop error" \
                  -message [lindex $result 1]

    stepper_button_state_control normal
  }
}


proc stepper_run_until {} {

  global stepper_tutor
  global stop_type
  global run_until_time
  global step_now
  global current_stepper_id


  if $stepper_tutor {
    tk_messageBox -icon info -type ok -title "Tutoring" \
                  -message "Run Until not allowed when in tutor mode."
  } else {

    stepper_button_state_control disabled

    set result [call_act_r_command "step-stepper" nil [list $current_stepper_id false $stop_type $run_until_time]]


    if {[lindex $result 0] == "null"} {
      tk_messageBox -icon info -type ok -title "Run Until error" \
                    -message [lindex $result 1]

      stepper_button_state_control normal
    }
  }
}  

proc update_instantiation_viewers {list} {
  global stepper_tutor
  global tutor_bindings
  global stepper_tutored 
  global tutor_answers

  if {[$list curselection] != ""} {
  
    set data [call_act_r_command "update-stepper" nil [list [$list get [$list curselection]]]]

    set params [lindex $data 0]
    set bindings [lindex $data 1]
    set display [lindex $data 2]
    set tutored_display [lindex $data 3]


    update_text_pane .stepper.prod_frame.f5.f.text $params

    update_text_pane .stepper.prod_frame.f3.f.text $display

    if {$stepper_tutored == 1} {

      set b ""
      set line 1

      foreach i $tutored_display {
     
        set var_name [lindex $i 0]
        set strt 1.0
        set side lhs
        set buf "no"
        set match "$var_name"  
                  # " $var_name"
        set is_buffer 0


        if {$i != "null"} {
          append b [format "%s: \n" [lindex $i 0]]
        }

        if [regexp {^\'(.*)\'$} [lindex $i 1] all sub] {
          set val [format "\"%s\"" $sub]
        } else {
          set val [lindex $i 1]
        }

        set tutor_answers([lindex $i 0]) [list $val 0 $line]
        incr line


        if {[string tolower [lindex $i 2]] == "true"} {
          set is_buffer 1
        } 

        while {[set indx [.stepper.prod_frame.f3.f.text search $match $strt end]] != ""} { 

          set v_start $indx 
                      # [.stepper.prod_frame.f3.f.text index "$indx + 1 chars"]

          set word_end [.stepper.prod_frame.f3.f.text index "$v_start + [string length $var_name] chars"]

          if {[.stepper.prod_frame.f3.f.text search -backwards -exact "==>" $v_start 1.0] != ""} {

            set side "rhs"
          } elseif {$buf == "no"} {
            set previous_buf [.stepper.prod_frame.f3.f.text search -backwards -regexp " =.*>" $v_start 1.0]
            if {$previous_buf != ""} {
              set b_start [.stepper.prod_frame.f3.f.text index "$previous_buf + 1 chars"]
              set b_end [.stepper.prod_frame.f3.f.text search ">" $b_start end]
              set buf [.stepper.prod_frame.f3.f.text get $b_start $b_end]
            }
          }
          
          if { $side == "rhs" && $is_buffer == 1 && [.stepper.prod_frame.f3.f.text search -exact ">" $word_end "$word_end +1 chars"] != ""} {

          #do nothing for the RHS buffer modification actions

          } else {
          set t_name [new_variable_name tag]

          .stepper.prod_frame.f3.f.text tag add $t_name $v_start $word_end
          .stepper.prod_frame.f3.f.text tag configure $t_name -background black -foreground white 

          set tutor_bindings($t_name) [list $v_start $word_end $var_name $side $is_buffer $buf]

          .stepper.prod_frame.f3.f.text tag bind $t_name <1> {
            global tutor_bindings
            global tutor_ans

            set t_name [%W tag names @%x,%y]

            if {[llength $t_name] == 1} {
              set strt [lindex $tutor_bindings($t_name) 0]
              set w_end [lindex $tutor_bindings($t_name) 1]
              set var [lindex $tutor_bindings($t_name) 2]
              set side [lindex $tutor_bindings($t_name) 3]
              set is_buf [lindex $tutor_bindings($t_name) 4]
              set buf [lindex $tutor_bindings($t_name) 5]

              if {[get_tutor_response $var $side $buf $is_buf $strt $w_end $t_name]} {

                tkwait variable tutor_ans

                if {$tutor_ans != "NoAnswer"} {

                  array unset tutor_bindings [lindex $tutor_ans 3]

                  .stepper.prod_frame.f3.f.text configure -state normal
                  .stepper.prod_frame.f3.f.text delete [lindex $tutor_ans 1] [lindex $tutor_ans 2]
                  .stepper.prod_frame.f3.f.text tag delete [lindex $tutor_ans 3]
                  .stepper.prod_frame.f3.f.text insert [lindex $tutor_ans 1] [lindex $tutor_ans 0]
                  .stepper.prod_frame.f3.f.text configure -state disabled
                }
              }
            } else {
              tk_messageBox -icon warning -title "Invalid Selection" \
                            -message "There was additional text selected when clicking on a variable.  Please try again." -type ok
            }
          }
          }
          set strt $word_end
          
        }
      }
      update_text_pane .stepper.prod_frame.f2.f.text $b
    } else {
      update_text_pane .stepper.prod_frame.f2.f.text $bindings
    }
  }
} 

proc get_tutor_response {word side buf is_buf start end name} {
  if {[winfo exists .tutor_response] == 1} {
    tk_messageBox -icon info -type ok -title "Tutoring" \
                  -message "You haven't completed the previous binding yet."
    wm deiconify .tutor_response
    raise .tutor_response

    return 0
  } else {

    global tutor_help
    global tutor_entry
    global tutor_ans

    toplevel .tutor_response
    wm withdraw .tutor_response
    wm title .tutor_response "Tutor Response"

    wm geometry .tutor_response [get_configuration .tutor_response]

    label .tutor_response.label \
          -text "What is the binding for $word?" \
          -justify left -font label_font

    entry .tutor_response.entry \
          -width 50 -textvariable tutor_entry -font text_font

    set tutor_entry ""

    set tutor_ans ""

    bind .tutor_response.entry <Key-Return> "accept_tutor_response $word $start $end $name"
    
    label .tutor_response.help \
          -textvariable tutor_help \
          -justify left -font label_font \
          -height 2
    
    set tutor_help ""

    button .tutor_response.help_button -text "Help" -font button_font -command "tutor_help $word"
    button .tutor_response.hint_button -text "Hint" -font button_font -command "tutor_hint $word $side $buf $is_buf"

    bind .tutor_response.entry <Destroy> {
      if {$tutor_ans == ""} {
        set tutor_ans "NoAnswer"
      }
    }    

    pack .tutor_response.label -anchor w
    pack .tutor_response.entry -anchor w
    pack .tutor_response.help -anchor w
    pack .tutor_response.hint_button -side left
    pack .tutor_response.help_button -side left

    wm deiconify .tutor_response

    after idle {focus .tutor_response.entry}

    return 1
  }
}

proc accept_tutor_response {word start end name} {
  global tutor_entry
  global tutor_help
  global tutor_ans
  global tutor_answers

  if {$tutor_entry != ""} {

    set correct [lindex $tutor_answers($word) 0]

    if {[string compare -nocase $correct [string trim $tutor_entry]] == 0} {

      set tutor_ans [list $correct $start $end $name]
      destroy .tutor_response

      if {[lindex $tutor_answers($word) 1] == 0} {
        
        set i [string length "$word : "]
        set l [lindex $tutor_answers($word) 2]
 
        .stepper.prod_frame.f2.f.text insert "$l.$i" $correct
        
        set tutor_answers($word) [list $correct 1]
      }

    } else {
      set tutor_help "Incorrect.\n$tutor_entry is not the binding for $word in this instantiation."
      set tutor_entry ""
    }
  }
}


proc tutor_hint {word side buf is_buf} {
  global tutor_help
  global tutor_answers
  
  if {[lindex $tutor_answers($word) 1] == 1} {
    set tutor_help "Look in the bindings section of the stepper window\nto see the current binding for $word."
  } elseif {$side == "rhs"} {
    set tutor_help "You should find the binding for $word on\nthe left hand side of the production first."
  } elseif $is_buf {
    set tutor_help "Use the buffers tool to determine the chunk in the [string range $word 1 end] buffer."
  } elseif {$side == "lhs" && $buf != ""} { 
    set tutor_help "$word is in a slot of the [string range $buf 1 end] buffer.\nYou can find its value using the buffers tool."
  } else {
    set tutor_help "No hint is available for this variable.  If it is in a !bind! or !mv-bind! you will need to use the help button to get the answer."
  }
}

proc tutor_help {word} {
  global tutor_help
  global tutor_answers

  set tutor_help "The binding of $word is [lindex $tutor_answers($word) 0]"
}

proc check_conflict_resolution {} {

  global tutor_bindings
  global tutor_selections
  global tutor_cr_none
 
  set data $tutor_bindings(productions)

  set picked 0

  for {set count 0} {$count < [llength $data]} {incr count} {
    if $tutor_selections($count) {
      incr picked
    }
  }

  if {$tutor_cr_none == 0 && $picked == 0} {
  


   tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "You must select at least one production or the None match option."

  } else {
    set count 0
    set correct 0

    foreach i $data {

      .tutor_cr.frame.c.contents.$count.but.b configure -state disabled

      set value [lindex $i 1]

      pack forget .tutor_cr.frame.c.contents.$count

      incr count
    }

    set count 0

    foreach i $data {

      set value [lindex $i 1]

      if $tutor_selections($count) {
        if {$value == ""} {
          incr correct
        } else {
          .tutor_cr.frame.c.contents.$count.label configure -width [winfo width .tutor_cr.frame.c.contents.$count.but.text]
          .tutor_cr.frame.c.contents.$count.label configure -text "Does not match.\n[lindex $i 1]"
          pack .tutor_cr.frame.c.contents.$count

          update_text_pane .tutor_cr.frame.c.contents.$count.but.text [.tutor_cr.frame.c.contents.$count.but.text get 1.0 end]
        }
      } else {
        if {$value != ""} {
          incr correct
        } else {

          .tutor_cr.frame.c.contents.$count.label configure -width [winfo width .tutor_cr.frame.c.contents.$count.but.text]

          .tutor_cr.frame.c.contents.$count.label configure -text "Does match the current state."

          pack .tutor_cr.frame.c.contents.$count
          
          update_text_pane .tutor_cr.frame.c.contents.$count.but.text [.tutor_cr.frame.c.contents.$count.but.text get 1.0 end]

        }
      }
      incr count
    }

  if {$correct == [llength $data]} {
    destroy .tutor_cr
  } else {
    destroy .tutor_cr.none
  .tutor_cr.message configure -text "These selections were incorrect. Press Ok after reviewing the reasons."
  .tutor_cr.button configure -command "destroy .tutor_cr" -text "Ok"
  }
}
}

proc unselect_all_productions {} {

  global tutor_bindings
  global tutor_cr_none

  if {$tutor_cr_none == 1} {
    for {set c 0} {$c < [llength $tutor_bindings(productions)]} {incr c} {
  
      .tutor_cr.frame.c.contents.$c.but.b deselect
    }
  }
}

proc display_conflict_resolution {} {
  global tutor_bindings
  global tutor_selections

  if [array exists tutor_selections] {
    array unset tutor_selections
  } 

  set data $tutor_bindings(productions)

  if {[winfo exists .tutor_cr] == 1} {
    wm deiconify .tutor_cr
    raise .tutor_cr

  } else {

    toplevel .tutor_cr
    wm withdraw .tutor_cr
    wm title .tutor_cr "Conflict Resolution"

    wm geometry .tutor_cr [get_configuration .tutor_cr]

    message .tutor_cr.message \
          -text "Using the information in the buffers, click the box next to each of the productions which have conditions that match the current state, or check the box labeled 'None match' if none of them match.  Then Press the Check button when done." \
          -font label_font


    place .tutor_cr.message -x 2 -y 5 -width -4 -relwidth 1.0 -height 100

    button .tutor_cr.button -text "Check" -font button_font -command check_conflict_resolution
    
    place .tutor_cr.button -x -40 -relx .5 -width 80 -height 25 -y 110


    checkbutton .tutor_cr.none -text "None match" -command unselect_all_productions -variable tutor_cr_none

    .tutor_cr.none deselect

    place .tutor_cr.none -x 2 -y 140 -height 20 

    
    bind .tutor_cr <Destroy> {
        global tutor_bindings
        global tutor_selections
        array unset tutor_bindings
        array unset tutor_selections
    }    


    # Use a simple scrollable frame which is based on code
    # found at http://www.tek-tips.com/viewthread.cfm?qid=372792 by 
    # Ken Jones of Avia Training and Consulting, www.avia-training.com

    # Create a "hull" frame to contain all other widgets 
    # composing the scrollable frame.

    frame .tutor_cr.frame
 
    canvas .tutor_cr.frame.c -height [winfo height .tutor_cr]  -width [winfo width .tutor_cr] -yscrollcommand ".tutor_cr.frame.ybar set"
    scrollbar .tutor_cr.frame.ybar -orient vertical -command ".tutor_cr.frame.c yview"
 
    # Create the frame that will actually
    # hold all children of the scrollable
    # frame. All children should be packed
    # or gridded into this frame, *not* the
    # hull frame or the canvas!

    frame .tutor_cr.frame.c.contents -borderwidth 0
 
    # Tell the canvas to display the frame,
    # anchoring the upper-left hand corner
    # of the frame in the upper-left hand
    # corner of the canvas
 
    .tutor_cr.frame.c create window 0 0 -anchor nw -window .tutor_cr.frame.c.contents -tag win
 
    # Use grid to display the canvas and its
    # scrollbars. Handle resizing properly.
 
    grid .tutor_cr.frame.c -row 0 -column 0 -sticky nsew -pady 4 -padx 2
    grid .tutor_cr.frame.ybar -row 0 -column 1 -sticky ns -pady 4 -padx 2
    grid columnconfigure .tutor_cr.frame 0 -weight 1
    grid rowconfigure .tutor_cr.frame 0 -weight 1
 
    # Detect <Configure> events on the frame
    # to detect when it is first displayed
    # and any time is is resized. In either
    # case, recompute the visible bounds of
    # the canvas and update its -scrollregion
    # attribute.
 

    bind .tutor_cr.frame <Configure> {
      .tutor_cr.message configure -width [winfo width .tutor_cr.frame.c]

      .tutor_cr.frame.c configure -scrollregion [.tutor_cr.frame.c bbox all]
      .tutor_cr.frame.c itemconfigure win -width [winfo width .tutor_cr.frame.c]
      .tutor_cr.frame.c itemconfigure win -height [winfo height .tutor_cr.frame.c]
    }

    bind .tutor_cr.frame.c.contents <Configure> {

      .tutor_cr.message configure -width [winfo width .tutor_cr.frame.c]
      .tutor_cr.frame.c configure -scrollregion [.tutor_cr.frame.c bbox all]
      .tutor_cr.frame.c itemconfigure win -width [winfo width .tutor_cr.frame.c]
      .tutor_cr.frame.c itemconfigure win -height [winfo height .tutor_cr.frame.c]
    }

    place .tutor_cr.frame -x 0 -y 160 -relwidth 1.0 -relheight 1.0 -height -160


    set count 0

    foreach i $data {

      frame .tutor_cr.frame.c.contents.$count -borderwidth 0
      frame .tutor_cr.frame.c.contents.$count.but -borderwidth 0

      checkbutton .tutor_cr.frame.c.contents.$count.but.b -variable tutor_selections($count) -command ".tutor_cr.none deselect"

      .tutor_cr.frame.c.contents.$count.but.b deselect

      text .tutor_cr.frame.c.contents.$count.but.text -state disabled -font text_font -width 45 -height [expr 1 + [llength [split [lindex $i 0] "\n"]]]
 
      update_text_pane .tutor_cr.frame.c.contents.$count.but.text "(p production[expr 1 + $count]\n[lindex $i 0]==>"

      message .tutor_cr.frame.c.contents.$count.label -text "" -font label_font -justify left

      # grid config .tutor_cr.frame.c.contents.$count.text -column 1 -row 0 -sticky nsew

      # grid config .tutor_cr.frame.c.contents.$count.label -column 1 -row 1 -sticky w

      # grid .tutor_cr.frame.c.contents.$count.b -column 0 -row 0 -sticky nsew


      pack .tutor_cr.frame.c.contents.$count.but.b -side left
      pack .tutor_cr.frame.c.contents.$count.but.text -side right
      pack .tutor_cr.frame.c.contents.$count.but
      pack .tutor_cr.frame.c.contents.$count.label



      pack .tutor_cr.frame.c.contents.$count -anchor nw

      
      incr count
    }

    wm deiconify .tutor_cr

  }

}


proc check_start_retrieval {} {

  global tutor_bindings
  global tutor_selections
  global tutor_r_none
 
  set data $tutor_bindings(retrieval)

  set picked 0

  for {set count 0} {$count < [llength [lindex $data 1]]} {incr count} {
    if $tutor_selections($count) {
      incr picked
    }
  }

  if {$tutor_r_none == 0 && $picked == 0} {

   tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "You must select at least one chunk or the None match option."

  } else {

    set count 0
    set correct 0

    foreach i [lindex $data 1] {

      .tutor_r.frame.c.contents.$count.but.b configure -state disabled

      set value [lindex $i 1]

      pack forget .tutor_r.frame.c.contents.$count
      
      incr count
    }

    set count 0

    foreach i [lindex $data 1] {

      set value [lindex $i 1]

      if $tutor_selections($count) {
        if {$value == true} {
          incr correct
        } else {
          .tutor_r.frame.c.contents.$count.label configure -width [winfo width .tutor_r.frame.c.contents.$count.but.text]
          .tutor_r.frame.c.contents.$count.label configure -text "Does not match the retrieval request."
          pack .tutor_r.frame.c.contents.$count

          update_text_pane .tutor_r.frame.c.contents.$count.but.text [.tutor_r.frame.c.contents.$count.but.text get 1.0 end]
        }
      } else {
        if {$value == "null"} {
          incr correct
        } else {
          .tutor_r.frame.c.contents.$count.label configure -width [winfo width .tutor_r.frame.c.contents.$count.but.text]
          .tutor_r.frame.c.contents.$count.label configure -text "Does match the retrieval request."
          pack .tutor_r.frame.c.contents.$count
          update_text_pane .tutor_r.frame.c.contents.$count.but.text [.tutor_r.frame.c.contents.$count.but.text get 1.0 end]
        }
      }
      incr count
    }

    if {$correct == [llength [lindex $data 1]]} {
      destroy .tutor_r
    } else {
      destroy .tutor_r.none
      .tutor_r.message configure -text "These selections were incorrect. Press Ok after reviewing the results.\n\n[lindex $data 0]"
      .tutor_r.button configure -command "destroy .tutor_r" -text "Ok"
    }
  }
}

proc unselect_all_retrievals {} {

  global tutor_bindings
  global tutor_r_none

  if {$tutor_r_none == 1} {
    for {set c 0} {$c < [llength [lindex $tutor_bindings(retrieval) 1]]} {incr c} {
  
      .tutor_r.frame.c.contents.$c.but.b deselect
    }
  }
}

proc display_retrieval_choice {} {
  global tutor_bindings
  global tutor_selections

  if [array exists tutor_selections] {
    array unset tutor_selections
  } 

  set data $tutor_bindings(retrieval)

  if {[winfo exists .tutor_r] == 1} {
    wm deiconify .tutor_r
    raise .tutor_r

  } else {

    toplevel .tutor_r
    wm withdraw .tutor_r
    wm title .tutor_r "Start Retrieval"

    wm geometry .tutor_r [get_configuration .tutor_r]

    message .tutor_r.message \
          -text "Click the box next to each chunk from the model's declarative memory which matches the retrieval request from the production that fired (shown below) or check the 'None match' box if none of them match. Then press the Check button when done.\n\n[lindex $data 0]" \
          -font label_font

    place .tutor_r.message -x 2 -y 5 -width -4 -relwidth 1.0 -height 150

    button .tutor_r.button -text "Check" -font button_font -command check_start_retrieval
    
    place .tutor_r.button -x -40 -relx .5 -width 80 -height 25 -y 160

    checkbutton .tutor_r.none -text "None match" -command unselect_all_retrievals -variable tutor_r_none

    .tutor_r.none deselect

    place .tutor_r.none -x 2 -y 190 -height 20 



    bind .tutor_r <Destroy> {
        global tutor_bindings
        global tutor_selections
        array unset tutor_bindings
        array unset tutor_selections
    }    


    # Use a simple scrollable frame which is based on code
    # found at http://www.tek-tips.com/viewthread.cfm?qid=372792 by 
    # Ken Jones of Avia Training and Consulting, www.avia-training.com

    # Create a "hull" frame to contain all other widgets 
    # composing the scrollable frame.

    frame .tutor_r.frame
 
    canvas .tutor_r.frame.c -height [winfo height .tutor_r] -width [winfo width .tutor_r] -yscrollcommand ".tutor_r.frame.ybar set"
    scrollbar .tutor_r.frame.ybar -orient vertical -command ".tutor_r.frame.c yview"
 
    # Create the frame that will actually
    # hold all children of the scrollable
    # frame. All children should be packed
    # or gridded into this frame, *not* the
    # hull frame or the canvas!

    frame .tutor_r.frame.c.contents -borderwidth 0
 
    # Tell the canvas to display the frame,
    # anchoring the upper-left hand corner
    # of the frame in the upper-left hand
    # corner of the canvas
 
    .tutor_r.frame.c create window 0 0 -anchor nw -window .tutor_r.frame.c.contents -tag win
 
    # Use grid to display the canvas and its
    # scrollbars. Handle resizing properly.
 
    grid .tutor_r.frame.c -row 0 -column 0 -sticky nsew -pady 4 -padx 2
    grid .tutor_r.frame.ybar -row 0 -column 1 -sticky ns -pady 4 -padx 2
    grid columnconfigure .tutor_r.frame 0 -weight 1
    grid rowconfigure .tutor_r.frame 0 -weight 1
 
    # Detect <Configure> events on the frame
    # to detect when it is first displayed
    # and any time is is resized. In either
    # case, recompute the visible bounds of
    # the canvas and update its -scrollregion
    # attribute.
 

    bind .tutor_r.frame <Configure> {
      .tutor_r.message configure -width [winfo width .tutor_r.frame.c]
     
      .tutor_r.frame.c configure -scrollregion [.tutor_r.frame.c bbox all]
      .tutor_r.frame.c itemconfigure win -width [winfo width .tutor_r.frame.c]
      .tutor_r.frame.c itemconfigure win -height [winfo height .tutor_r.frame.c]
    }

    bind .tutor_r.frame.c.contents <Configure> {

      .tutor_r.message configure -width [winfo width .tutor_r.frame.c]
      .tutor_r.frame.c configure -scrollregion [.tutor_r.frame.c bbox all]
      .tutor_r.frame.c itemconfigure win -width [winfo width .tutor_r.frame.c]
      .tutor_r.frame.c itemconfigure win -height [winfo height .tutor_r.frame.c]
    }

    place .tutor_r.frame -x 0 -y 210 -relwidth 1.0 -relheight 1.0 -height -210


    set count 0

    foreach i [lindex $data 1] {

      frame .tutor_r.frame.c.contents.$count -borderwidth 0

      frame .tutor_r.frame.c.contents.$count.but -borderwidth 0

      checkbutton .tutor_r.frame.c.contents.$count.but.b -variable tutor_selections($count) -command ".tutor_r.none deselect"

      .tutor_r.frame.c.contents.$count.but.b deselect

      text .tutor_r.frame.c.contents.$count.but.text -state disabled -font text_font -width 45 -height [llength [split [lindex $i 0] "\n"]]
 
      update_text_pane .tutor_r.frame.c.contents.$count.but.text [lindex $i 0]

      message .tutor_r.frame.c.contents.$count.label -text "" -font label_font -justify left

#      grid config .tutor_r.frame.c.contents.$count.text -column 1 -row 0 -sticky nsew
#      grid config .tutor_r.frame.c.contents.$count.label -column 1 -row 1 -sticky w
#      grid .tutor_r.frame.c.contents.$count.b -column 0 -row 0 -sticky nsew

      pack .tutor_r.frame.c.contents.$count.but.b -side left
      pack .tutor_r.frame.c.contents.$count.but.text -side right
      pack .tutor_r.frame.c.contents.$count.but
      pack .tutor_r.frame.c.contents.$count.label

      pack .tutor_r.frame.c.contents.$count -anchor nw

      
      incr count
    }

    wm deiconify .tutor_r

  }

}





set stepper_wait [add_new_cmd "wait_for_stepper" "wait_for_stepper" "Internal command for stepper tool wait during pre-event hook. Do not call."]
set stepper_stepped [add_new_cmd "display_stepper_stepped" "display_stepper_stepped" "Internal command for stepper tool update. Do not call."]
set close_stepper [add_new_cmd "close-stepper-tool" "close_stepper_tool" "Internal command for closing the stepper when a clear-all occurs. Do not call."]
set reset_stepper [add_new_cmd "reset-stepper-tool" "reset_stepper_tool" "Internal command for monitoring resets in stepper. Do not call."]



button [control_panel_name].step_frame.step_button -command {select_stepper} -text "Stepper" -font button_font

pack [control_panel_name].step_frame.step_button -side left

pack [control_panel_name].step_frame.step_all_events -side right

pack [control_panel_name].step_frame

button [control_panel_name].pause_button -command {stepper_pause} -text "Pause" -font button_font

pack [control_panel_name].pause_button
