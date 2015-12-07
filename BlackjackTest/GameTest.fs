namespace Adacola.Blackjack.Test

open FsUnit
open FsCheck
open NUnit.Framework

[<TestFixture>]
module Game =
    open Adacola.Blackjack
    open Basis.Core

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
    let ``validateRuleにPenetration<0を渡すとFailureを返すこと`` () =
        let ruleGen = gen {
            let! rule = Arb.generate<GameRule>
            let! penetration = Arb.generate<float> |> Gen.suchThat ((>) 0.0)
            return { rule with Penetration = penetration }
        }
        Prop.forAll (Arb.fromGen ruleGen) (Game.validateRule >> Result.isFailure)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``validateRuleにPenetration>1渡すとFailureを返すこと`` () =
        let ruleGen = gen {
            let! rule = Arb.generate<GameRule>
            let! penetration = Arb.generate<float> |> Gen.suchThat ((<) 1.0)
            return { rule with Penetration = penetration }
        }
        Prop.forAll (Arb.fromGen ruleGen) (Game.validateRule >> Result.isFailure)
        |> Check.QuickThrowOnFailure
