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

    [<SetUp>]
    let setup() = registerGen.Force()

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

    let getCardListGen sum =
        let rec loop result = function
            | 0 -> result |> Gen.constant
            | sum -> gen {
                let maxNumber = min sum 10
                let! number = Gen.choose(1, maxNumber)
                let! rank =
                    match number with
                    | 1 -> Gen.constant AceRank
                    | 10 -> Gen.elements [NumberRank 10; JackRank; QueenRank; KingRank]
                    | x -> Gen.constant (NumberRank x)
                let! card = Arb.generate<Card>
                return! loop ({ card with Rank = rank }::result) (sum - number)
            }
        loop [] sum

    [<Test>]
    let ``playPlayerにBusted状態の手を渡すとBustedが返り、カードリストと状態は変化せず、playerStrategyは呼び出されないこと`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(22, 30)
            let! playerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOf
            return cards, { gameState with PlayerHands = playerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let mutable called = false
            let playerStrategy _ = called <- true; Hit
            Game.playPlayer playerStrategy cards gameState = (Busted, cards, gameState) && not called)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``playPlayerに必ずHitするstrategyを渡すとBustedが返ること`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(2, 21)
            let! playerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOfLength 20
            return cards, { gameState with PlayerHands = playerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let playerStrategy _ = Hit
            let score, cards', gameState' = Game.playPlayer playerStrategy cards gameState
            score = Busted
            && List.rev cards' @ gameState'.PlayerHands = List.rev cards @ gameState.PlayerHands
            && gameState'.DealerHands = gameState.DealerHands)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``playPlayerに必ずStandするstrategyを渡すと初期stateが返ること`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(2, 21)
            let! playerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOfLength 20
            return cards, { gameState with PlayerHands = playerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let playerStrategy _ = Stand
            let score, cards', gameState' = Game.playPlayer playerStrategy cards gameState
            let expectedScore, _ = gameState.PlayerHands |> List.map Card.toNumber |> Score.calculate
            score = expectedScore && cards' = cards && gameState' = gameState)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``playDealerにBusted状態の手を渡すとBustedが返り、状態は変化せず、dealerStrategyは呼び出されないこと`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(22, 30)
            let! dealerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOf
            return cards, { gameState with DealerHands = dealerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let mutable called = false
            let dealerStrategy _ = called <- true; Game.DealerAction.Hit
            Game.playDealer dealerStrategy cards gameState = (Busted, cards, gameState) && not called)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``playDealerに必ずHitするstrategyを渡すとBustedが返ること`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(2, 21)
            let! dealerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOfLength 20
            return cards, { gameState with DealerHands = dealerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let dealerStrategy _ = Game.DealerAction.Hit
            let score, cards', gameState' = Game.playDealer dealerStrategy cards gameState
            score = Busted
            && List.rev cards' @ gameState'.DealerHands = List.rev cards @ gameState.DealerHands
            && gameState'.PlayerHands = gameState.PlayerHands)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``playDealerに必ずStandするstrategyを渡すと初期stateが返ること`` () =
        let parameterGen = gen {
            let! gameState = Arb.generate<GameState>
            let! sum = Gen.choose(2, 21)
            let! dealerHands = getCardListGen sum
            let! cards = Arb.generate<Card> |> Gen.listOfLength 20
            return cards, { gameState with DealerHands = dealerHands }
        }
        Prop.forAll (Arb.fromGen parameterGen) (fun (cards, gameState) ->
            let dealerStrategy _ = Game.DealerAction.Stand
            let score, cards', gameState' = Game.playDealer dealerStrategy cards gameState
            let expectedScore, _ = gameState.DealerHands |> List.map Card.toNumber |> Score.calculate
            score = expectedScore && cards' = cards && gameState' = gameState)
        |> Check.QuickThrowOnFailure

    // TODO getResultのテスト追加
    // TODO getGainのテスト追加
    // TODO playのテスト追加
