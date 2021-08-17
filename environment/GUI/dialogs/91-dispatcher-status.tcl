
proc make_dispatcher_status_viewer {} {
   
  if {[winfo exists .dispatcher] == 1} {
    wm deiconify .dispatcher
    raise .dispatcher
  } else {

    toplevel .dispatcher
    wm withdraw .dispatcher
    wm title .dispatcher "Connections Status"

    wm geometry .dispatcher [get_configuration .dispatcher]

  
    # frame and list box for names

    set list_frame_1 [frame .dispatcher.list_frame_1 -borderwidth 0]  
  
    set list_box_1 [listbox $list_frame_1.list_box -listvar \
                            $list_frame_1.list_box.var \
                            -yscrollcommand "$list_frame_1.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

  
    set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl -command "$list_box_1 yview"]

    # Frame and list box for commands
 
    set list_frame_2 [frame .dispatcher.list_frame -borderwidth 0]  
  
    set list_box_2 [listbox $list_frame_2.list_box -listvar \
                            $list_frame_2.list_box.var \
                            -yscrollcommand "$list_frame_2.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

  
    set list_scroll_bar_2 [scrollbar $list_frame_2.list_scrl -command "$list_box_2 yview"]


    # The lables for the sections 

    set l1 [label .dispatcher.l1 -text "Address" -justify center -font label_font]
    set l2 [label .dispatcher.l2 -text "Pending" -justify center -font label_font]
    set l3 [label .dispatcher.l3 -text "Sent" -justify center -font label_font]
    set l4 [label .dispatcher.l4 -text "Received" -justify center -font label_font]
    set l5 [label .dispatcher.l5 -text "Commands" -justify center -font label_font]

    # the labels for the info

    set li0 [label .dispatcher.li0 -text "" -font label_font -justify center -textvariable .dispatcher.li0.value]
    set li1 [label .dispatcher.li1 -text "" -font label_font -justify center -textvariable .dispatcher.li1.value]
    set li2 [label .dispatcher.li2 -text "" -font label_font -justify center -textvariable .dispatcher.li2.value]
    
    # Label and text box to hold the doc strings when a command is clicked

    set d1 [label .dispatcher.d1 -text "Documentation:" -justify left -font label_font]

    set d2 [label .dispatcher.d2 -font text_font -text "" -textvar .dispatcher.d2.value -justify left -anchor nw]

    global .dispatcher.d2.value 
    set .dispatcher.d2.value ""

    # make name selection update the other displays and create a global to
    # hold the data when the button is pressed

    global dispatcher_data
    set dispatcher_data ""

    global .dispatcher.li0.value
    global .dispatcher.li1.value
    global .dispatcher.li2.value

    .dispatcher.list_frame_1.list_box delete 0 end
    .dispatcher.list_frame.list_box delete 0 end
    set .dispatcher.li0.value ""
    set .dispatcher.li1.value ""
    set .dispatcher.li2.value ""
    set .dispatcher.d2.value ""

    bind $list_box_1 <<ListboxSelect>> "select_dispatcher_name"

    bind $list_box_2 <<ListboxSelect>> "select_command_name"

    bind $d2 <Configure> "resize_documentation %w"

    button .dispatcher.get -text "Get Status" -font button_font -command "get_dispatcher_status"

    bind $list_box_1 <Destroy> "global dispatcher_data
                                unset dispatcher_data" 

   
    pack $list_scroll_bar_1 -side right -fill y 
    pack $list_box_1 -side left -expand 1 -fill both

    pack $list_scroll_bar_2 -side right -fill y 
    pack $list_box_2 -side left -expand 1 -fill both

    place .dispatcher.get -relx 0 -y 0 -height 25 -relwidth .3
    place $list_frame_1 -relx 0 -y 25 -relheight 1.0 -height -85 -relwidth .3

    place $d1 -relx 0 -rely 1.0 -y -43 -height 25 -relwidth .3


    place $l1 -relx .3 -y 0 -height 25 -relwidth .7
    place $li0 -relx .3 -y 25 -height 25 -relwidth .7
    place $l2 -relx .3 -y 50 -height 25 -relwidth .7
    place $l4 -relx .3 -y 75 -height 25 -relwidth .35
    place $l3 -relx .65 -y 75 -height 25 -relwidth .35
    place $li1 -relx .3 -y 100 -height 25 -relwidth .35
    place $li2 -relx .65 -y 100 -height 25 -relwidth .35

    place $l5 -relx .3 -y 125 -height 25 -relwidth .7
    place $list_frame_2 -relx .3 -y 150 -relheight 1.0 -height -210 -relwidth .7


    place $d2 -relx .3 -rely 1.0 -y -60 -height 60 -relwidth .7

    # now show the window 

    wm deiconify .dispatcher
    focus .dispatcher

    return .dispatcher
  }
}


proc get_dispatcher_status {} {

  global dispatcher_data
  global .dispatcher.li0.value
  global .dispatcher.li1.value
  global .dispatcher.li2.value
  global .dispatcher.d2.value

  .dispatcher.list_frame_1.list_box delete 0 end
  .dispatcher.list_frame.list_box delete 0 end
  set .dispatcher.li0.value ""
  set .dispatcher.li1.value ""
  set .dispatcher.li2.value ""
  set .dispatcher.d2.value ""

  set dispatcher_data [lindex [send_cmd {list-connections} ""] 0]


  foreach i $dispatcher_data {
  
    .dispatcher.list_frame_1.list_box insert end [lindex $i 0]
  }
}


proc select_command_name {} {

  global .dispatcher.d2.value

  set .dispatcher.d2.value ""

  set selections [.dispatcher.list_frame.list_box curselection]

  if {[llength $selections] != 0} {
    set cmd [.dispatcher.list_frame.list_box get [lindex $selections 0]]
    
    set result [send_cmd "check" [list $cmd]]
 
    set w [expr "floor(.7 * [winfo width .dispatcher])"]

    .dispatcher.d2 configure -wraplength $w

    set .dispatcher.d2.value [lindex $result 2]

  }
}

proc resize_documentation {w} {

  .dispatcher.d2 configure -wraplength $w
}




proc select_dispatcher_name {} {

  global dispatcher_data
  global .dispatcher.li0.value
  global .dispatcher.li1.value
  global .dispatcher.li2.value
  global .dispatcher.d2.value

  .dispatcher.list_frame.list_box delete 0 end
  set .dispatcher.li0.value ""
  set .dispatcher.li1.value ""
  set .dispatcher.li2.value ""
  set .dispatcher.d2.value ""

  set selections [.dispatcher.list_frame_1.list_box curselection]

  if {[llength $selections] != 0} {
    set data [lindex $dispatcher_data [lindex $selections 0]]

    set .dispatcher.li0.value [lindex $data 1]
    set .dispatcher.li1.value [lindex $data 2]
    set .dispatcher.li2.value [lindex $data 3]

    foreach i [lindex $data 4] {

      .dispatcher.list_frame.list_box insert end $i
    }
  }
}


if {$tcl_platform(os) == "Darwin"} {
  button [control_panel_name].dispatcher_status -command make_dispatcher_status_viewer -text "Connections & Commands" -font button_font -justify center
} else {
  button [control_panel_name].dispatcher_status -command make_dispatcher_status_viewer -text "Connections &\nCommands" -font button_font -justify center
}

pack [control_panel_name].dispatcher_status
