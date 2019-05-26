let exec_get db id =
  match db with
    | None -> (db, Data.Null)
    | Some tree -> begin
        match B_tree.find tree id with
          | None -> (db, Data.Null)
          | Some (_, value) -> (db, value)
      end

let exec_command db command =
  match command with
    | Data.Get id -> exec_get db id
    | Data.Set (_, _) -> (db, Data.Number 1.)
    | Data.Put value -> B_tree.put db value
