namespace Adacola.Blackjack.Test

open FsCheck
open Adacola.Blackjack

[<AutoOpen>]
module Generator =

    let cardGen = gen {
        let! suit = Arb.generate<CardSuit>
        let! rank = AceRank::JackRank::QueenRank::KingRank::[for i in 2 .. 10 -> NumberRank i] |> Gen.elements
        return { Suit = suit; Rank = rank }
    }

    type BlackjackGenerator =
        static member Card() = Arb.fromGen cardGen

    let registerGen = lazy (Arb.register<BlackjackGenerator>() |> ignore)
