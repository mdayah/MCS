 CLS

 WakeupTime$ = "08:00:00"

 OPEN "COM1:9600,N,8,1,BIN" FOR OUTPUT AS #1 ' Open the COM port
 LOCATE 10, 10, 0: PRINT "Current Time: ";
 LOCATE 11, 10, 0: PRINT " Wakeup Time: "; WakeupTime$;

 DO
    LOCATE 10, 24, 0: PRINT TIME$;
 LOOP WHILE TIME$ < WakeupTime$
 
 PRINT #1, "ATDT 1-865-595-7851" + CHR$(13)

