type 'a b_tree_item = int * 'a

type 'a b_tree =
  | Node of 'a b_tree_item * 'a b_tree * 'a b_tree
  | Leaf of 'a b_tree_item

let item_of_b_tree tree =
  match tree with
    | Leaf(item) -> item
    | Node(item, _, _) -> item

let rec find tree key =
  match tree with
    | Leaf(k, v) ->
        if k == key then (k, v)
        else
          raise Not_found
    | Node(head, l, r) ->
        let (k, v) = head in
        if k == key then (k, v)
        else
          let (k, v) = item_of_b_tree l in
          if k <= key then
            find l key
          else
            find r key
