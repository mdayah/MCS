TYPE Old
     Part1 AS STRING * 72
     Part2 AS STRING * 1424
END TYPE

TYPE New
     Part1  AS STRING * 72
     Cel(5) AS LONG
     Part2  AS STRING * 1424
END TYPE

REM $INCLUDE: '..\Include\Activity.REC'

    DIM O AS Old
    DIM N AS New

    OPEN "..\DataFile\Activity.Mas" FOR RANDOM AS #1 LEN = LEN(O)
    OPEN "..\DataFile\Activity.New" FOR RANDOM AS #2 LEN = LEN(N)

    PRINT LEN(O)
    PRINT LEN(N)
    
    GET #1, 1, O

    N.Part1 = O.Part1
    N.Part2 = O.Part2

    FOR x = 0 TO 5
        N.Cel(x) = 0
    NEXT
    PUT #2, 1, N

    CLOSE

KILL "..\DataFile\Activity.Mas"
NAME "..\DataFile\Activity.New" AS "..\DataFile\Activity.Mas"

