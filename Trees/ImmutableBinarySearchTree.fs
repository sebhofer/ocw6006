module Trees.ImmutableBinarySearchTree

type Node<'key, 'value> =
    { Key: 'key
      Value: 'value
      Parent: Node<'key, 'value> option
      Left: Node<'key, 'value> option
      Right: Node<'key, 'value> option }

let left (node: Node<_, 'data>) = node.Left

let right (node: Node<_, 'data>) = node.Right

let value (node: Node<_, 'data>) = node.Value

let private createLeaf parent (key, value) =
    { Key = key
      Value = value
      Parent = Some parent
      Left = None
      Right = None }

let createRoot (key, value) =
    { Key = key
      Value = value
      Parent = None
      Left = None
      Right = None }

let rec insert (root: Node<'key, 'value>) (keyValue: 'key * 'value) =
    let conditionalInsert parent subtree =
        match subtree with
        | Some subtree -> insert subtree keyValue
        | None -> createLeaf parent keyValue
        |> Some

    let key, _ = keyValue

    if key <= root.Key then
        { root with
            Left = conditionalInsert root root.Left }
    else
        { root with
            Right = conditionalInsert root root.Right }

let ofList (items: ('key * 'value) list) =
    List.fold insert (createRoot items[0]) items[1..]

// move to Sequence
let inorderAccumulation (node: Node<'key, 'data>) =
    let rec walk node (acc: 'data list) =
        match node with
        | { Left = Some left
            Right = None
            Value = data } -> data :: (walk left acc)
        | { Left = Some left
            Right = Some right
            Value = data } -> data :: (walk left acc) |> walk right
        | { Left = None
            Right = Some right
            Value = data } -> walk right (data :: acc)
        | { Left = None
            Right = None
            Value = data } -> data :: acc

    walk node []

let first (node: Node<_, _>) =
    let rec walk node =
        match node.Left with
        | Some left -> walk left
        | None -> node

    walk node

let last (node: Node<_, _>) =
    let rec walk node =
        match node.Right with
        | Some right -> walk right
        | None -> node

    walk node

let rec tryFind key (node: Node<'key, 'value>) =
    if node.Key = key then
        Some node
    elif key < node.Key then
        node.Left |> Option.bind (tryFind key)
    else
        node.Right |> Option.bind (tryFind key)

let rec trySuccessor (node: Node<_, _>) =
    match node.Right with
    | Some right -> Some(first right)
    | None -> Option.bind trySuccessor node.Parent

let rec tryPredecessor (node: Node<_, _>) =
    match node.Left with
    | Some left -> Some(last left)
    | None -> Option.bind tryPredecessor node.Parent
