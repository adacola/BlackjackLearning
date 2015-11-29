namespace Adacola.Blackjack

/// ブラックジャックで使用するカードの数値
type CardNumber =
    /// 1または11として使用できるカードAを表します
    | Ace
    /// A以外の数字として使用するカードです。J、Q、KもNumber(10)として扱います。Numberの数値には2～10が入る想定です。
    | Number of int

/// カードのランク。トランプとして表示する際の情報として使用します。
type CardRank =
    | AceRank
    | JackRank
    | QueenRank
    | KingRank
    | NumberRank of int

/// カードのスート。トランプとして表示する際の情報として使用します。
type CardSuit =
    | Spades
    | Hearts
    | Diamonds
    | Clubs

/// カードの情報
type Card = {
    Suit : CardSuit
    Rank : CardRank
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Card =
    open System
    open FsRandom

    /// カードが妥当かどうかをチェックします
    let isValid = function
        | Ace -> true
        | Number x -> 2 <= x && x <= 10

    /// カードが取りうる数値のリストを取得します。
    /// Aの場合は1と11、Number(x)の場合はxのみのリストとなります。
    let numbers = function
        | Ace -> [1; 11]
        | Number x -> [x]

    /// カードからブラックジャックにおける数値に変換します
    let toNumber { Rank = r } =
        match r with
        | AceRank -> Ace
        | JackRank | QueenRank | KingRank -> Number 10
        | NumberRank n -> Number n

    /// シャッフルされていない52枚のカードセット
    let private cardSet =
        let numbers = Array.init 9 ((+) 2 >> NumberRank)
        let specials = [|AceRank; JackRank; QueenRank; KingRank|]
        let ranks = Array.append numbers specials
        [|  for s in [Spades; Hearts; Diamonds; Clubs] do
            for r in ranks ->
                { Suit = s; Rank = r } |]

    /// デッキ数と乱数を指定して、シャッフルされたカードセットを作成します
    let generateSet randomState deck =
        random {
            let cards = cardSet |> Array.replicate deck |> Array.concat
            do! Array.shuffleInPlace cards
            return cards
        } |> Random.get <| randomState

    /// デッキ数を指定して、シャッフルされた52枚のカードセットを作成します。
    /// 乱数アルゴリズムにはMT、seedには現在時刻が使用されます。
    let generateSetByDefault deck =
        DateTime.UtcNow.Ticks |> uint64 |> MersenneTwister.StateVector.Initialize
        |> createState MersenneTwister.mersenne
        |> generateSet <| deck
