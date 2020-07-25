module Config exposing (..)

-- Game Config
config : { gameSpeed : number, bet : number, initialPlayerCredit : number }
config = { gameSpeed = 1000, bet = 100, initialPlayerCredit = 1000 }

-- Game Odds
odds : Int -> Int
odds odd =
    case odd of
        6 ->
            10000

        7 ->
            7500

        8 ->
            5000

        9 ->
            2500

        10 ->
            1000

        11 ->
            500

        12 ->
            300

        13 ->
            200

        14 ->
            150

        15 ->
            100

        16 ->
            90

        17 ->
            80

        18 ->
            70

        19 ->
            60

        20 ->
            50

        21 ->
            40

        22 ->
            30

        23 ->
            25

        24 ->
            20

        25 ->
            15

        26 ->
            10

        27 ->
            9

        28 ->
            8

        29 ->
            7

        30 ->
            6

        31 ->
            5

        32 ->
            4

        33 ->
            3

        34 ->
            2

        35 ->
            1

        _ ->
            0
