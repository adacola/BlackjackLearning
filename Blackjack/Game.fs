namespace Adacola.Blackjack

/// ゲームの状態
type GameState = {
    /// 賭金
    Bet : int
    /// プレイヤーの手
    PlayerHands : Card list
    /// ディーラーの公開されている手
    DealerHands : Card list
}

/// プレイヤーが使用できる行動
type PlayerAction = Hit | Stand

/// ディーラーの行動方法
[<RequireQualifiedAccess>]
type DealerStrategy = StandOnAll17 | HitOnSoft17

/// ゲームの勝敗。プレイヤー目線での結果。
type GameResult =  Lose | Draw | Win of isBlackjack : bool

/// ゲームの最終的な状態
type GameConclusiveState = {
    /// ゲームの勝敗
    Result : GameResult
    /// プレイヤーの収支。プレイヤーの負の場合は賭金分のマイナス、引き分けの場合は0、プレイヤーの勝ちの場合は払い戻し額
    Gain : int
    /// ゲーム終了時の状態
    State : GameState
}

type GameRule = {
    /// ディーラーの行動方法
    DealerStrategy : DealerStrategy
    /// プレイヤーがBlackjackで勝利した場合の払い戻し率。1.5または1.2のケースが多いです。
    PayRatioWinningBlackjack : float
    /// デッキ数
    Deck : int
}

module Game =
    open Adacola.Blackjack
    open Basis.Core
    open System

    // ディーラーが使用できる行動
    [<RequireQualifiedAccess>]
    type internal DealerAction = Hit | Stand

    let internal dealerStrategyStandOnAll17 = function
        | Busted, _ -> failwith "Bustedでは何もできない"
        | x, _ when x < Score 17 -> DealerAction.Hit
        | _ -> DealerAction.Stand

    let internal dealerStrategyHitOnSoft17 = function
        | Busted, _ -> failwith "Bustedでは何もできない"
        | Score 17, true -> DealerAction.Hit
        | x, _ when x < Score 17 -> DealerAction.Hit
        | _ -> DealerAction.Stand

    let validateRule rule =
        let { PayRatioWinningBlackjack = payRatio; Deck = deck } = rule
        if payRatio < 1.0 then ArgumentException(sprintf "PayRatioWinnerBlackjack(%f)は1以上である必要があります" payRatio) |> Failure
        elif deck < 1 then ArgumentException(sprintf "Deck(%d)は1以上である必要があります" deck) |> Failure
        else Success ()

    let rec internal playPlayer playerStrategy cards gameState =
        let { PlayerHands = hands } = gameState
        match hands |> List.map Card.toNumber |> Score.calculate |> fst with
        | Busted -> Busted, cards, gameState
        | score ->
            match playerStrategy gameState, cards with
            | Hit, card::rest ->
                { gameState with PlayerHands = card::hands } |> playPlayer playerStrategy rest
            | Hit, [] -> failwith "カードが残っていません"
            | Stand, _ -> score, cards, gameState

    let rec internal playDealer dealerStrategy cards gameState =
        let { DealerHands = hands } = gameState
        match hands |> List.map Card.toNumber |> Score.calculate with
        | Busted, _ -> Busted, cards, gameState
        | score ->
            match dealerStrategy score, cards with
            | DealerAction.Hit, card::rest ->
                { gameState with DealerHands = card::hands } |> playDealer dealerStrategy rest
            | DealerAction.Hit, [] -> failwith "カードが残っていません"
            | DealerAction.Stand, _ -> fst score, cards, gameState

    let internal getResult playerScore dealerScore =
        match Score.compare playerScore dealerScore with
        | 1 -> Win(playerScore = Blackjack)
        | -1 -> Lose
        | _ -> Draw

    let internal getGain payRatioWinningBlackjack bet = function
        | Win true -> float bet * payRatioWinningBlackjack |> int
        | Win false -> bet
        | Draw -> 0
        | Lose -> -bet

    /// <summary>
    /// ゲームをプレイします。
    /// </summary>
    /// <param name="rule">ゲームルール</param>
    /// <param name="playerStrategy">プレイヤーの行動戦略</param>
    /// <param name="bet">賭け金の額</param>
    /// <returns>ゲーム結果</returns>
    let play rule playerStrategy bet =
        rule |> validateRule |> Result.iterFailure raise
        let dealerStrategy =
            match rule.DealerStrategy with
            | DealerStrategy.StandOnAll17 -> dealerStrategyStandOnAll17
            | DealerStrategy.HitOnSoft17 -> dealerStrategyHitOnSoft17
        
        // デッキの最初の2枚はディーラー用、そのうち最初の1枚は公開情報。3枚目以降はプレイヤーのカード
        let cards = Card.generateSetByDefault rule.Deck
        let dealerUpCard = cards.[0]
        let dealerClosedCard = cards.[1]
        let playerCards = [cards.[3]; cards.[2]]
        let playerStartCards = cards.[4 ..] |> Array.toList

        // プレイヤーがゲームプレイ
        let playerStartGameState = { Bet = bet; PlayerHands = playerCards; DealerHands = [dealerUpCard] }
        let playerScore, playerEndCards, playerEndGameState = playPlayer playerStrategy playerStartCards playerStartGameState
        // ディーラーがゲームプレイ
        let dealerStartGameState = { playerEndGameState with DealerHands = dealerClosedCard::playerEndGameState.DealerHands }
        let dealerScore, _, dealerEndGameState = playDealer dealerStrategy playerEndCards dealerStartGameState

        // 勝敗を決定
        let result = getResult playerScore dealerScore
        let gain = result |> getGain rule.PayRatioWinningBlackjack bet
        { Result = result; Gain = gain; State = dealerEndGameState }
