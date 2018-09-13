module App.View

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fable.PowerPack
open Fable.PowerPack.Fetch


type Model =
    { PokemonDisplayModel : Pokemon.Model }


type Msg =
    | UpdatePokemonDisplay of Pokemon.Msg


let init _ =
    let pokemonModel, pokemonCmds =
        Pokemon.init ()
    { PokemonDisplayModel = pokemonModel },
    Cmd.map UpdatePokemonDisplay pokemonCmds

let update msg model =
    match msg with
    | UpdatePokemonDisplay pmsg ->
        let pokemonModel, pokemonCmds =
            Pokemon.update pmsg model.PokemonDisplayModel
        { model with PokemonDisplayModel = pokemonModel },
        Cmd.map UpdatePokemonDisplay pokemonCmds


let view model dispatch =
    let pokemonView =
        Pokemon.view model.PokemonDisplayModel
            (fun m -> dispatch (UpdatePokemonDisplay m))
    Container.container [] [ pokemonView ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
