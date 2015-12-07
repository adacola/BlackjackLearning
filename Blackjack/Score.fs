namespace Adacola.Blackjack

/// カードの数値の合計値を表します
type Score =
    /// バーストした状態。いかなるスコアより低くなります。
    | Busted
    /// バーストしていない状態。21が最高スコアとなります。
    | Score of int
    /// ブラックジャック(2枚で21)の状態。Score(21, _)より高いスコアとなります。
    | Blackjack

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Score =

    /// スコアを計算します。
    /// 戻り値 : (スコア, ソフトかどうか(ソフトの場合はtrue))
    let calculate = function
        | [Ace; Number 10] | [Number 10; Ace] -> Blackjack, true
        | cards ->
            let aceCount, numberSum =
                ((0, 0), cards) ||> List.fold (fun (aceCount, numberSum) -> function
                    | Ace -> aceCount + 1, numberSum
                    | Number n -> aceCount, numberSum + n)
            let inline toScore x = if 21 < x then Busted else Score x
            let softAceScore = numberSum + (if aceCount = 0 then 0 else 11 + aceCount - 1) |> toScore
            let hardAceScore = numberSum + aceCount |> toScore
            // 「hardAceScore = softAceScore」となるケースはAceがない場合か両方バーストの場合なのでソフトではない
            // 「hardAceScore > softAceScore」となるケースはsoftAceScoreがバーストしてhardAceScoreはバーストしていない場合なのでソフトではない
            if hardAceScore < softAceScore then softAceScore, true else hardAceScore, false


    /// <summary>
    /// プレイヤーとディーラーのスコアを比較します。
    /// プレイヤーとディーラーのスコアの大小はScore同士を単純に比較しても正しく得られないため、この関数を使用してください。
    /// </summary>
    /// <param name="playerScore">プレイヤーのスコア</param>
    /// <param name="dealerScore">ディーラーのスコア</param>
    /// <returns>プレイヤーの方が勝っている場合は1、ディーラーの方が勝っている場合は-1、引き分けの場合は0</returns>
    let compare playerScore dealerScore =
        match playerScore, dealerScore with
        | Busted, Busted -> -1
        | p, d -> compare p d
