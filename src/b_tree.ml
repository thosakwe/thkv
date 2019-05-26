type 'a b_tree_item = int * 'a

type 'a b_tree =
  | Node of 'a b_tree_item * 'a b_tree * 'a b_tree
  | Leaf of 'a b_tree_item

let item_of_b_tree tree =
  match tree with
    | Leaf(item) -> item
    | Node(item, _, _) -> item

let rec size_of_b_tree tree =
  match tree with
    | None -> 0
    | Some tree -> begin
        match tree with
          | Leaf _ -> 1
          | Node(_, l, r) -> 1 + (size_of_b_tree (Some l)) + (size_of_b_tree (Some r))
    end

let rec find tree key =
  match tree with
    | Leaf(k, v) ->
        if k == key then Some (k, v)
        else
          None
    | Node(head, l, r) ->
        let (k, v) = head in
        if k == key then Some (k, v)
        else
          let (k, _) = item_of_b_tree l in
          if k <= key then
            find l key
          else
            find r key

let put tree value =
  match tree with
   | None -> (Some (Leaf (0, value)), value)
   | Some tree -> (Some tree, value)
