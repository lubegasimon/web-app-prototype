module Datastore = struct
  let hashtbl = Hashtbl.create 10
  let create_user (name : string) (id : string) = Hashtbl.add hashtbl name id
  let find_user (name : string) = Hashtbl.find_opt hashtbl name
  let remove_user (name : string) = Hashtbl.remove hashtbl name

  let update_user (name : string) (id : string) =
    Hashtbl.replace hashtbl name id
end
