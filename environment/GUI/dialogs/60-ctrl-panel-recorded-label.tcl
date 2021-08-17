label [control_panel_name].gen_recorded_label -text "General\nRecordable Data" -font label_font

pack [control_panel_name].gen_recorded_label



proc get_incremental_history {id} {

  set start [call_act_r_command "get-incremental-history-data" nil [list $id]]
  set result ""

  while {$start != ""} {
   
      set result "$result [json::json2dict [lindex $start 0]]"
 
      set start [call_act_r_command "get-incremental-history-data" nil [list $id]]
  }
  return $result
}
