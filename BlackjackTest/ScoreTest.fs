namespace Adacola.Blackjack.Test

open FsUnit
open FsCheck
open NUnit.Framework

[<TestFixture>]
module Score =
    open Adacola.Blackjack
    open System

    [<Test>]
    let ``同じScore同士の=が常に成り立つこと`` () =
        (fun (score : Score) -> score = score)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Busted < Scoreが常に成り立つこと`` () =
        (fun score -> Busted < Score score)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Score < Blackjackが常に成り立つこと`` () =
        (fun score -> Score score < Blackjack)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Score(x)において、xの大小とScore(x)の大小が一致すること`` () =
        (fun score1 score2 -> compare (Score score1) (Score score2) = compare score1 score2)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Busted同士のcompareでディーラーが勝つこと`` () =
        Score.compare Busted Busted |> should be (lessThan 0)

    [<Test>]
    let ``Busted以外の同スコアのcompareで引き分けになること`` () =
        (fun score -> (score <> Busted) ==> (Score.compare score score = 0))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``異なるスコアのcompare結果がScoreのcompare結果と一致すること`` () =
        (fun score1 score2 -> (score1 <> score2) ==> (Score.compare score1 score2 = compare score1 score2))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``calculateに空リストを渡した場合は(Score(0), false)が返ること`` () =
        [] |> Score.calculate |> should equal (Score 0, false)

    [<Test>]
    let ``calculateにAceとNumber(10)を渡した場合は(Blackjack, true)が返ること`` () =
        [Ace; Number 10] |> Score.calculate |> should equal (Blackjack, true)
        [Number 10; Ace] |> Score.calculate |> should equal (Blackjack, true)

    [<Test>]
    let ``calculateにAceを含まないで21以下のxになるリストを渡した場合は(Score(x), false)が返ること`` () =
        let getCardListWithoutAceGen sum =
            let rec loop result = function
                | 0 -> result |> List.map Number |> Gen.constant
                | 1 -> invalidArg "sum" "1は不正です"
                | sum -> gen {
                    let maxNumber = min sum 10
                    let! number = Gen.choose(2, maxNumber) |> Gen.suchThat (fun n -> sum - n <> 1)
                    return! loop (number::result) (sum - number)
                }
            loop [] sum
        let cardListWithoutAceGen = gen {
            let! sum = Gen.choose(2, 21)
            let! numbers = getCardListWithoutAceGen sum
            return sum, numbers
        }
        Prop.forAll (Arb.fromGen cardListWithoutAceGen) (fun (sum, cards) -> Score.calculate cards = (Score sum, false))
        |> Check.QuickThrowOnFailure

    let getCardListGen sum =
        let rec loop result = function
            | 0 -> result |> List.map (function 1 -> Ace | x -> Number x) |> Gen.constant
            | sum -> gen {
                let maxNumber = min sum 10
                let! number = Gen.choose(1, maxNumber)
                return! loop (number::result) (sum - number)
            }
        loop [] sum

    [<Test>]
    let ``calculateにAceを含んでBlackjackでなくSoft xになるリストを渡した場合は(Score(x), true)が返ること`` () =
        let cardListGen = gen {
            let! sum = Gen.choose(0, 10)
            let! numbers = getCardListGen sum |> Gen.suchThat ((<>) [Number 10]) |> Gen.map ResizeArray
            let! insertIndex = Gen.choose(0, numbers.Count)
            do numbers.Insert(insertIndex, Ace)
            return sum + 11, numbers |> Seq.toList
        }
        Prop.forAll (Arb.fromGen cardListGen) (fun (sum, cards) -> Score.calculate cards = (Score sum, true))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``calculateにAceを含んでHard xになるリストを渡した場合は(Score(x), false)が返ること`` () =
        let cardListGen = gen {
            let! sum = Gen.choose(11, 20)
            let! numbers = getCardListGen sum |> Gen.map ResizeArray
            let! insertIndex = Gen.choose(0, numbers.Count)
            do numbers.Insert(insertIndex, Ace)
            return sum + 1, numbers |> Seq.toList
        }
        Prop.forAll (Arb.fromGen cardListGen) (fun (sum, cards) -> Score.calculate cards = (Score sum, false))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``21を超えるリストを渡した場合は(Busted, false)が返ること`` () =
        let cardListGen = gen {
            let! sum = Gen.frequency [9, Gen.choose(22, 30); 1, Gen.choose(31, 100)]
            let! numbers = getCardListGen sum
            return numbers
        }
        Prop.forAll (Arb.fromGen cardListGen) (Score.calculate >> (=) (Busted, false))
        |> Check.QuickThrowOnFailure
