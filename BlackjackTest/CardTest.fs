namespace Adacola.Blackjack.Test

open FsUnit
open FsCheck
open NUnit.Framework

[<TestFixture>]
module Card =
    open Adacola.Blackjack
    open System
    open FsRandom

    [<SetUp>]
    let setup() = registerGen.Force()

    [<Test>]
    let ``Aceの場合はisValidがtrueを返すこと`` () =
        Ace |> Card.isValid |> should be True

    [<Test>]
    let ``Number(2)～Number(10)の場合はisValidがtrueを返すこと`` () =
        [2 .. 10] |> List.forall (Number >> Card.isValid) |> should be True

    [<Test>]
    let ``Number(2)～Number(10)以外の場合はisValidがfalseを返すこと`` () =
        let numberGen = gen {
            let under2 = Gen.frequency [9, Gen.choose(Int32.MinValue, 0); 1, Gen.constant 1]
            let over10 = Gen.frequency [9, Gen.choose(12, Int32.MaxValue); 1, Gen.constant 11]
            let! number = Gen.oneof [under2; over10]
            return Number number
        }
        Prop.forAll (Arb.fromGen numberGen) (Card.isValid >> not)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Aceの場合はnumbersが[1; 11]を返すこと`` () =
        Ace |> Card.numbers |> should equal [1; 11]

    [<Test>]
    let ``Number(x)の場合はnumbersが[x]を返すこと`` () =
        let expected = [for i in 2 .. 10 -> [i]]
        [2 .. 10] |> List.map (Number >> Card.numbers) |> should equal expected

    [<Test>]
    let ``toNumberでカードを数値に変換できること`` () =
        let cardGen = gen {
            let! card = Arb.generate<Card>
            let! rank, number =
                [AceRank, Ace; JackRank, Number 10; QueenRank, Number 10; KingRank, Number 10]
                @ [for i in 2 .. 10 -> NumberRank i, Number i]
                |> Gen.elements
            return { card with Rank = rank }, number
        }
        Prop.forAll (Arb.fromGen cardGen) (fun (card, expected) -> Card.toNumber card = expected)
        |> Check.QuickThrowOnFailure

    let getExpectedSortedCards deck =
        let ranks = [AceRank; JackRank; QueenRank; KingRank] @ [for i in 2 .. 10 -> NumberRank i]
        [|  for s in [Spades; Hearts; Diamonds; Clubs] do
            for r in ranks do
            for _ in 1 .. deck -> {Suit = s; Rank = r} |]
        |> Array.sort

    [<Test>]
    let ``generateSetでランダムな並びのカードセットが生成されること`` () =
        let stateGen = gen {
            let! seed = Arb.from<uint64> |> Arb.toGen
            let! deck = Gen.choose(1, 10)
            return seed, deck
        }
        Prop.forAll (Arb.fromGen stateGen) (fun (seed, deck) ->
            let expectedCardLength = deck * 52
            let expectedSortedCards = getExpectedSortedCards deck
            let randomState = seed |> MersenneTwister.StateVector.Initialize |> createState MersenneTwister.mersenne
            let cards = Card.generateSet randomState deck
            cards.Length = expectedCardLength && cards <> expectedSortedCards && Array.sort cards = expectedSortedCards)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``generateSetByDefaultでランダムな並びのカードセットが生成されること`` () =
        let deckGen = gen {
            return! Gen.choose(1, 10)
        }
        Prop.forAll (Arb.fromGen deckGen) (fun deck ->
            let expectedCardLength = deck * 52
            let expectedSortedCards = getExpectedSortedCards deck
            let cards = Card.generateSetByDefault deck
            cards.Length = expectedCardLength && cards <> expectedSortedCards && Array.sort cards = expectedSortedCards)
        |> Check.QuickThrowOnFailure
