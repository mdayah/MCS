    REM $INCLUDE: '..\Include\Receipts.Rec'

    COMMON SHARED Rct      AS Receipts   '19
    COMMON SHARED Rcx      AS RctIndex   '20
    COMMON SHARED Rcl      AS RctLine    '21


    CLOSE 19: OPEN "..\DataFile\Receipts.Mas" FOR RANDOM AS #19 LEN = LEN(Rct)
    CLOSE 20: OPEN "..\DataFile\Receipts.Inx" FOR RANDOM AS #20 LEN = LEN(Rcx)
    CLOSE 21: OPEN "..\DataFile\Receipts.Agr" FOR RANDOM AS #21 LEN = LEN(Rcx)

    PRINT LOF(19) / LEN(Rct)
    PRINT LOF(20) / 10
    PRINT LOF(21) / 10


    FOR x = 1 TO LOF(19) / LEN(Rct)
        GET #19, x, Rct
        GET #20, x, Rcx
        IF Rcx.Agr <> VAL(Rct.Agr) THEN PRINT Rcx.Agr; Rct.Agr,
        IF Rcx.Rct <> VAL(Rct.Num) THEN PRINT Rcx.Rct; Rct.Num,
        IF Rcx.Num <> x THEN PRINT Rcx.Num, x
    NEXT

