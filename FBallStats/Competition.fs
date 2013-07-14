module Competition

type State = {
    players : string list
}
with static member Zero = { players = [] }

type Command =
    | Add of string

type Event =
    | PlayerAdded of string

module private Assert =
    let contains name list = List.exists (fun elem -> elem = name) list
    let thatPlayerIsNew state name = if contains name state.players then invalidArg "name" "Player is already in competition" else (state, name)

let apply state = function
    | PlayerAdded name -> Assert.thatPlayerIsNew state name |> fun (state, name) -> { state with players = name :: state.players }

