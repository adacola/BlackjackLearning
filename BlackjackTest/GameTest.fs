namespace Adacola.Blackjack.Test

open FsUnit
open FsCheck
open NUnit.Framework

[<TestFixture>]
module Game =
    open Adacola.Blackjack
    open Basis.Core
    open System

    let getScoreGen withinBlackjack minScore maxScore = gen {
        let scores = [for i in minScore .. maxScore -> Score i]
        let patterns = if withinBlackjack then Blackjack::scores else scores
        let! score = Gen.elements patterns
        let! isSoft = Arb.generate<bool>
        return score, isSoft
    }

    [<Test>]
    let ``dealerStrategyStandOnAll17はScore(16)以下の場合Hitすること`` () =
        Prop.forAll (getScoreGen false 1 16 |> Arb.fromGen) (Game.dealerStrategyStandOnAll17 >> (=) Game.DealerAction.Hit)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``dealerStrategyStandOnAll17はScore(17)以上の場合Standすること`` () =
        Prop.forAll (getScoreGen true 17 21 |> Arb.fromGen) (Game.dealerStrategyStandOnAll17 >> (=) Game.DealerAction.Stand)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``dealerStrategyStandOnAll17はBustedの場合例外が発生すること`` () =
        fun () -> Game.dealerStrategyStandOnAll17 (Busted, false) |> ignore
        |> should throw typeof<exn>
        fun () -> Game.dealerStrategyStandOnAll17 (Busted, true) |> ignore
        |> should throw typeof<exn>

    [<Test>]
    let ``dealerStrategyHitOnSoft17はScore(16)以下の場合Hitすること`` () =
        Prop.forAll (getScoreGen false 1 16 |> Arb.fromGen) (Game.dealerStrategyHitOnSoft17 >> (=) Game.DealerAction.Hit)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``dealerStrategyHitOnSoft17はScore(18)以上の場合Standすること`` () =
        Prop.forAll (getScoreGen true 18 21 |> Arb.fromGen) (Game.dealerStrategyHitOnSoft17 >> (=) Game.DealerAction.Stand)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``dealerStrategyHitOnSoft17はHard17の場合Standすること`` () =
        Game.dealerStrategyHitOnSoft17 (Score 17, false) |> should equal Game.DealerAction.Stand

    [<Test>]
    let ``dealerStrategyHitOnSoft17はSoft17の場合Standすること`` () =
        Game.dealerStrategyHitOnSoft17 (Score 17, true) |> should equal Game.DealerAction.Hit

    [<Test>]
    let ``dealerStrategyHitOnSoft17はBustedの場合例外が発生すること`` () =
        fun () -> Game.dealerStrategyHitOnSoft17 (Busted, false) |> ignore
        |> should throw typeof<exn>
        fun () -> Game.dealerStrategyHitOnSoft17 (Busted, true) |> ignore
        |> should throw typeof<exn>

    [<Test>]
    let ``validateRuleにPayRatioWinningBlackjack<1を渡すとFailureを返すこと`` () =
        let ruleGen = gen {
            let! rule = Arb.generate<GameRule>
            let! payRatio = Arb.generate<float> |> Gen.suchThat ((>) 1.0)
            return { rule with PayRatioWinningBlackjack = payRatio }
        }
        Prop.forAll (Arb.fromGen ruleGen) (Game.validateRule >> Result.isFailure)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``validateRuleにDeck<1渡すとFailureを返すこと`` () =
        let ruleGen = gen {
            let! rule = Arb.generate<GameRule>
            let! deck = Gen.frequency [1, Gen.constant 0; 9, Gen.choose(Int32.MinValue, -1)]
            return { rule with Deck = deck }
        }
        Prop.forAll (Arb.fromGen ruleGen) (Game.validateRule >> Result.isFailure)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``validateRuleに妥当なGameRuleを渡すとSuccessを返すこと`` () =
        let ruleGen = gen {
            let! payRatio = Arb.generate<float> |> Gen.suchThat ((<=) 1.0)
            let! deck = Gen.frequency [1, Gen.constant 1; 9, Gen.choose(2, Int32.MaxValue)]
            let! dealerStrategy = Arb.generate<DealerStrategy>
            return {
                PayRatioWinningBlackjack = payRatio
                Deck = deck
                DealerStrategy = dealerStrategy }
        }
        Prop.forAll (Arb.fromGen ruleGen) (Game.validateRule >> (=) (Success()))
        |> Check.QuickThrowOnFailure
