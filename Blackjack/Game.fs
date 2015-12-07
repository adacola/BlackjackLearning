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
    /// デッキのうちどの程度ゲームに使用するか
    Penetration : float
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

    let validateRule { PayRatioWinningBlackjack = payRatio; Penetration = penetration } =
        if payRatio < 1.0 then Failure(ArgumentException(sprintf "PayRatioWinnerBlackjack(%f)は1以上である必要があります" payRatio))
        elif penetration < 0.0 || 1.0 < penetration then Failure(ArgumentException(sprintf "Penetration(%f)は0～1の間である必要があります" penetration))
        else Success ()

    let play rule (playerStrategy : GameState -> PlayerAction) bet =
        rule |> validateRule |> Result.iterFailure raise
        let dealerStrategyFunc =
            List.map Card.toNumber >> Score.calculate
            >> match rule.DealerStrategy with
                | DealerStrategy.StandOnAll17 -> dealerStrategyStandOnAll17
                | DealerStrategy.HitOnSoft17 -> dealerStrategyHitOnSoft17
        // TODO
        System.NotImplementedException() |> raise
