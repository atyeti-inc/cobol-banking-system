IDENTIFICATION DIVISION.
PROGRAM-ID. DATEUTIL.
*> ================================================================
*> DATEUTIL - Date Utility Functions
*> BDAY = Add business days (perpetual Federal Reserve holiday calendar)
*> WKDY = Weekday check (Monday-Friday)
*> DIFF = Day difference (Julian Day Number)
*> LEAP = Leap year check
*> ISBD = Business day check (weekday and not federal bank holiday)
*> HOLI = Federal bank holiday check
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
*> Date decomposition fields
01  WS-YEAR                       PIC 9(4).
01  WS-MONTH                      PIC 9(2).
01  WS-DAY                        PIC 9(2).
01  WS-YEAR2                      PIC 9(4).
01  WS-MONTH2                     PIC 9(2).
01  WS-DAY2                       PIC 9(2).

*> Zeller / day-of-week computation
01  WS-ZELLER-Y                   PIC 9(8).
01  WS-ZELLER-M                   PIC 9(4).
01  WS-ZELLER-H                   PIC 9(8).
01  WS-ZELLER-TEMP                PIC 9(8).
01  WS-DOW                        PIC 9(1).
    88 WS-DOW-SATURDAY            VALUE 0.
    88 WS-DOW-SUNDAY              VALUE 1.
    88 WS-DOW-MONDAY              VALUE 2.
    88 WS-DOW-THURSDAY            VALUE 5.
    88 WS-DOW-FRIDAY              VALUE 6.

*> Julian Day Number computation
01  WS-JDN1                       PIC 9(10).
01  WS-JDN2                       PIC 9(10).
01  WS-JDN-Y                      PIC 9(8).
01  WS-JDN-M                      PIC 9(4).
01  WS-JDN-TEMP                   PIC 9(10).

*> BDAY working fields
01  WS-BDAY-REMAINING             PIC S9(4).
01  WS-BDAY-CURRENT-DATE          PIC 9(8).
01  WS-BDAY-CUR-YEAR              PIC 9(4).
01  WS-BDAY-CUR-MONTH             PIC 9(2).
01  WS-BDAY-CUR-DAY               PIC 9(2).
01  WS-DAYS-IN-MONTH              PIC 9(2).
01  WS-IS-LEAP                    PIC 9(1).

*> Holiday detection
01  WS-IS-HOLIDAY                 PIC 9(1).

*> DIFF result (signed)
01  WS-DIFF-RESULT                PIC S9(10).

LINKAGE SECTION.
01  LS-FUNCTION                   PIC X(4).
01  LS-DATE-INPUT.
    05  LS-DATE1                  PIC 9(8).
    05  LS-DATE2                  PIC 9(8).
    05  LS-DAYS-TO-ADD            PIC S9(4).
01  LS-DATE-OUTPUT.
    05  LS-RESULT-DATE            PIC 9(8).
    05  LS-RESULT-DAYS            PIC S9(8).
    05  LS-RESULT-FLAG            PIC X(1).
01  LS-DATE-RESULT.
    05  LS-DATE-RESULT-CODE       PIC X(5).
    05  LS-DATE-RESULT-MSG        PIC X(50).

PROCEDURE DIVISION USING LS-FUNCTION
                         LS-DATE-INPUT
                         LS-DATE-OUTPUT
                         LS-DATE-RESULT.
MAIN-LOGIC.
    EVALUATE LS-FUNCTION
        WHEN "BDAY"
            PERFORM DO-BDAY
        WHEN "WKDY"
            PERFORM DO-WKDY
        WHEN "DIFF"
            PERFORM DO-DIFF
        WHEN "LEAP"
            PERFORM DO-LEAP
        WHEN "ISBD"
            PERFORM DO-ISBD
        WHEN "HOLI"
            PERFORM DO-HOLI
        WHEN OTHER
            MOVE "E0001" TO LS-DATE-RESULT-CODE
            MOVE "Invalid function code" TO LS-DATE-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ================================================================
*> BDAY - Add business days to a date
*> ================================================================
DO-BDAY.
    IF LS-DAYS-TO-ADD < 0
        MOVE "E0002" TO LS-DATE-RESULT-CODE
        MOVE "Negative business days not supported"
            TO LS-DATE-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE LS-DATE1 TO WS-BDAY-CURRENT-DATE
    MOVE LS-DAYS-TO-ADD TO WS-BDAY-REMAINING

    PERFORM UNTIL WS-BDAY-REMAINING <= 0
        PERFORM ADVANCE-ONE-CALENDAR-DAY
        PERFORM GET-DOW-FOR-BDAY
        IF NOT WS-DOW-SATURDAY AND NOT WS-DOW-SUNDAY
            PERFORM CHECK-HOLIDAY
            IF WS-IS-HOLIDAY = 0
                SUBTRACT 1 FROM WS-BDAY-REMAINING
            END-IF
        END-IF
    END-PERFORM

    MOVE WS-BDAY-CURRENT-DATE TO LS-RESULT-DATE
    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ----------------------------------------------------------------
*> Advance WS-BDAY-CURRENT-DATE by one calendar day
*> ----------------------------------------------------------------
ADVANCE-ONE-CALENDAR-DAY.
    DIVIDE WS-BDAY-CURRENT-DATE BY 10000
        GIVING WS-BDAY-CUR-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-BDAY-CUR-MONTH REMAINDER WS-BDAY-CUR-DAY

    PERFORM CALC-DAYS-IN-MONTH

    ADD 1 TO WS-BDAY-CUR-DAY
    IF WS-BDAY-CUR-DAY > WS-DAYS-IN-MONTH
        MOVE 1 TO WS-BDAY-CUR-DAY
        ADD 1 TO WS-BDAY-CUR-MONTH
        IF WS-BDAY-CUR-MONTH > 12
            MOVE 1 TO WS-BDAY-CUR-MONTH
            ADD 1 TO WS-BDAY-CUR-YEAR
        END-IF
    END-IF

    COMPUTE WS-BDAY-CURRENT-DATE =
        WS-BDAY-CUR-YEAR * 10000
        + WS-BDAY-CUR-MONTH * 100
        + WS-BDAY-CUR-DAY.

*> ----------------------------------------------------------------
*> Get day-of-week for WS-BDAY-CURRENT-DATE -> WS-DOW
*> ----------------------------------------------------------------
GET-DOW-FOR-BDAY.
    DIVIDE WS-BDAY-CURRENT-DATE BY 10000
        GIVING WS-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-MONTH REMAINDER WS-DAY
    PERFORM CALC-DOW.

*> ================================================================
*> CHECK-HOLIDAY - Algorithmic Rule Engine for Federal Bank Holidays
*> Covers all 11 Federal Reserve Bank Holidays perpetually (5 U.S.C. 6103)
*> Includes standard weekend observation shifts (Sat->Fri, Sun->Mon)
*> Input:  WS-BDAY-CUR-YEAR, WS-BDAY-CUR-MONTH, WS-BDAY-CUR-DAY, WS-DOW
*> Output: WS-IS-HOLIDAY (1=yes, 0=no)
*> ================================================================
CHECK-HOLIDAY.
    MOVE 0 TO WS-IS-HOLIDAY

    *> 1. New Year's Day (Jan 1) & Observations
    IF WS-BDAY-CUR-MONTH = 1 AND WS-BDAY-CUR-DAY = 1
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Jan 1 on Sunday -> Observed Monday Jan 2
    IF WS-BDAY-CUR-MONTH = 1 AND WS-BDAY-CUR-DAY = 2 AND WS-DOW-MONDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Jan 1 on Saturday -> Observed Friday Dec 31
    IF WS-BDAY-CUR-MONTH = 12 AND WS-BDAY-CUR-DAY = 31 AND WS-DOW-FRIDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 2. Martin Luther King Jr. Day (3rd Monday in January: 15-21)
    IF WS-BDAY-CUR-MONTH = 1 AND WS-DOW-MONDAY
        AND WS-BDAY-CUR-DAY >= 15 AND WS-BDAY-CUR-DAY <= 21
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 3. Presidents' Day / Washington's Birthday (3rd Monday in Feb: 15-21)
    IF WS-BDAY-CUR-MONTH = 2 AND WS-DOW-MONDAY
        AND WS-BDAY-CUR-DAY >= 15 AND WS-BDAY-CUR-DAY <= 21
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 4. Memorial Day (Last Monday in May: 25-31)
    IF WS-BDAY-CUR-MONTH = 5 AND WS-DOW-MONDAY
        AND WS-BDAY-CUR-DAY >= 25 AND WS-BDAY-CUR-DAY <= 31
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 5. Juneteenth National Independence Day (June 19) & Observations
    IF WS-BDAY-CUR-MONTH = 6 AND WS-BDAY-CUR-DAY = 19
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> June 19 on Saturday -> Observed Friday June 18
    IF WS-BDAY-CUR-MONTH = 6 AND WS-BDAY-CUR-DAY = 18 AND WS-DOW-FRIDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> June 19 on Sunday -> Observed Monday June 20
    IF WS-BDAY-CUR-MONTH = 6 AND WS-BDAY-CUR-DAY = 20 AND WS-DOW-MONDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 6. Independence Day (July 4) & Observations
    IF WS-BDAY-CUR-MONTH = 7 AND WS-BDAY-CUR-DAY = 4
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> July 4 on Saturday -> Observed Friday July 3
    IF WS-BDAY-CUR-MONTH = 7 AND WS-BDAY-CUR-DAY = 3 AND WS-DOW-FRIDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> July 4 on Sunday -> Observed Monday July 5
    IF WS-BDAY-CUR-MONTH = 7 AND WS-BDAY-CUR-DAY = 5 AND WS-DOW-MONDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 7. Labor Day (1st Monday in September: 1-7)
    IF WS-BDAY-CUR-MONTH = 9 AND WS-DOW-MONDAY
        AND WS-BDAY-CUR-DAY >= 1 AND WS-BDAY-CUR-DAY <= 7
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 8. Columbus Day (2nd Monday in October: 8-14)
    IF WS-BDAY-CUR-MONTH = 10 AND WS-DOW-MONDAY
        AND WS-BDAY-CUR-DAY >= 8 AND WS-BDAY-CUR-DAY <= 14
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 9. Veterans Day (November 11) & Observations
    IF WS-BDAY-CUR-MONTH = 11 AND WS-BDAY-CUR-DAY = 11
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Nov 11 on Saturday -> Observed Friday Nov 10
    IF WS-BDAY-CUR-MONTH = 11 AND WS-BDAY-CUR-DAY = 10 AND WS-DOW-FRIDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Nov 11 on Sunday -> Observed Monday Nov 12
    IF WS-BDAY-CUR-MONTH = 11 AND WS-BDAY-CUR-DAY = 12 AND WS-DOW-MONDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 10. Thanksgiving Day (4th Thursday in November: 22-28)
    IF WS-BDAY-CUR-MONTH = 11 AND WS-DOW-THURSDAY
        AND WS-BDAY-CUR-DAY >= 22 AND WS-BDAY-CUR-DAY <= 28
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF

    *> 11. Christmas Day (December 25) & Observations
    IF WS-BDAY-CUR-MONTH = 12 AND WS-BDAY-CUR-DAY = 25
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Dec 25 on Saturday -> Observed Friday Dec 24
    IF WS-BDAY-CUR-MONTH = 12 AND WS-BDAY-CUR-DAY = 24 AND WS-DOW-FRIDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF
    *> Dec 25 on Sunday -> Observed Monday Dec 26
    IF WS-BDAY-CUR-MONTH = 12 AND WS-BDAY-CUR-DAY = 26 AND WS-DOW-MONDAY
        MOVE 1 TO WS-IS-HOLIDAY
    END-IF.

*> ================================================================
*> WKDY - Check if date is a weekday (Monday - Friday)
*> ================================================================
DO-WKDY.
    DIVIDE LS-DATE1 BY 10000
        GIVING WS-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-MONTH REMAINDER WS-DAY
    PERFORM CALC-DOW
    IF WS-DOW-SATURDAY OR WS-DOW-SUNDAY
        MOVE "N" TO LS-RESULT-FLAG
    ELSE
        MOVE "Y" TO LS-RESULT-FLAG
    END-IF
    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ================================================================
*> ISBD - Check if date is a business day (weekday & not a holiday)
*> ================================================================
DO-ISBD.
    DIVIDE LS-DATE1 BY 10000
        GIVING WS-BDAY-CUR-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-BDAY-CUR-MONTH REMAINDER WS-BDAY-CUR-DAY
    MOVE WS-BDAY-CUR-YEAR TO WS-YEAR
    MOVE WS-BDAY-CUR-MONTH TO WS-MONTH
    MOVE WS-BDAY-CUR-DAY TO WS-DAY
    PERFORM CALC-DOW
    IF WS-DOW-SATURDAY OR WS-DOW-SUNDAY
        MOVE "N" TO LS-RESULT-FLAG
    ELSE
        PERFORM CHECK-HOLIDAY
        IF WS-IS-HOLIDAY = 1
            MOVE "N" TO LS-RESULT-FLAG
        ELSE
            MOVE "Y" TO LS-RESULT-FLAG
        END-IF
    END-IF
    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ================================================================
*> HOLI - Check if date is a federal bank holiday
*> ================================================================
DO-HOLI.
    DIVIDE LS-DATE1 BY 10000
        GIVING WS-BDAY-CUR-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-BDAY-CUR-MONTH REMAINDER WS-BDAY-CUR-DAY
    MOVE WS-BDAY-CUR-YEAR TO WS-YEAR
    MOVE WS-BDAY-CUR-MONTH TO WS-MONTH
    MOVE WS-BDAY-CUR-DAY TO WS-DAY
    PERFORM CALC-DOW
    PERFORM CHECK-HOLIDAY
    IF WS-IS-HOLIDAY = 1
        MOVE "Y" TO LS-RESULT-FLAG
    ELSE
        MOVE "N" TO LS-RESULT-FLAG
    END-IF
    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ================================================================
*> DIFF - Calculate difference in days between two dates
*> ================================================================
DO-DIFF.
    *> Convert DATE1 to JDN
    DIVIDE LS-DATE1 BY 10000
        GIVING WS-YEAR REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-MONTH REMAINDER WS-DAY
    PERFORM CALC-JDN-1

    *> Convert DATE2 to JDN
    DIVIDE LS-DATE2 BY 10000
        GIVING WS-YEAR2 REMAINDER WS-JDN-TEMP
    DIVIDE WS-JDN-TEMP BY 100
        GIVING WS-MONTH2 REMAINDER WS-DAY2
    MOVE WS-YEAR2 TO WS-YEAR
    MOVE WS-MONTH2 TO WS-MONTH
    MOVE WS-DAY2 TO WS-DAY
    PERFORM CALC-JDN-2

    COMPUTE WS-DIFF-RESULT = WS-JDN2 - WS-JDN1
    IF WS-DIFF-RESULT < 0
        COMPUTE WS-DIFF-RESULT = 0 - WS-DIFF-RESULT
    END-IF
    MOVE WS-DIFF-RESULT TO LS-RESULT-DAYS

    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ================================================================
*> LEAP - Check if year is a leap year
*> ================================================================
DO-LEAP.
    DIVIDE LS-DATE1 BY 10000
        GIVING WS-YEAR REMAINDER WS-JDN-TEMP

    PERFORM CHECK-LEAP-YEAR
    IF WS-IS-LEAP = 1
        MOVE "Y" TO LS-RESULT-FLAG
    ELSE
        MOVE "N" TO LS-RESULT-FLAG
    END-IF
    MOVE "E0000" TO LS-DATE-RESULT-CODE
    MOVE SPACES TO LS-DATE-RESULT-MSG.

*> ================================================================
*> CALC-DOW - Compute day of week using Zeller's congruence
*> Input:  WS-YEAR, WS-MONTH, WS-DAY
*> Output: WS-DOW (0=Sat,1=Sun,2=Mon,3=Tue,4=Wed,5=Thu,6=Fri)
*> ================================================================
CALC-DOW.
    MOVE WS-YEAR TO WS-ZELLER-Y
    MOVE WS-MONTH TO WS-ZELLER-M
    IF WS-ZELLER-M <= 2
        ADD 12 TO WS-ZELLER-M
        SUBTRACT 1 FROM WS-ZELLER-Y
    END-IF
    COMPUTE WS-ZELLER-TEMP =
        FUNCTION INTEGER(13 * (WS-ZELLER-M + 1) / 5)
    COMPUTE WS-ZELLER-H =
        WS-DAY + WS-ZELLER-TEMP
        + WS-ZELLER-Y
        + FUNCTION INTEGER(WS-ZELLER-Y / 4)
        - FUNCTION INTEGER(WS-ZELLER-Y / 100)
        + FUNCTION INTEGER(WS-ZELLER-Y / 400)
    COMPUTE WS-DOW =
        FUNCTION MOD(WS-ZELLER-H, 7).

*> ================================================================
*> CALC-JDN-1 - Compute Julian Day Number for date 1
*> Input:  WS-YEAR, WS-MONTH, WS-DAY
*> Output: WS-JDN1
*> ================================================================
CALC-JDN-1.
    MOVE WS-YEAR TO WS-JDN-Y
    MOVE WS-MONTH TO WS-JDN-M
    IF WS-JDN-M <= 2
        ADD 12 TO WS-JDN-M
        SUBTRACT 1 FROM WS-JDN-Y
    END-IF
    COMPUTE WS-JDN1 =
        WS-DAY
        + FUNCTION INTEGER((153 * (WS-JDN-M - 3) + 2) / 5)
        + 365 * WS-JDN-Y
        + FUNCTION INTEGER(WS-JDN-Y / 4)
        - FUNCTION INTEGER(WS-JDN-Y / 100)
        + FUNCTION INTEGER(WS-JDN-Y / 400)
        + 1721119.

*> ================================================================
*> CALC-JDN-2 - Compute Julian Day Number for date 2
*> Input:  WS-YEAR, WS-MONTH, WS-DAY
*> Output: WS-JDN2
*> ================================================================
CALC-JDN-2.
    MOVE WS-YEAR TO WS-JDN-Y
    MOVE WS-MONTH TO WS-JDN-M
    IF WS-JDN-M <= 2
        ADD 12 TO WS-JDN-M
        SUBTRACT 1 FROM WS-JDN-Y
    END-IF
    COMPUTE WS-JDN2 =
        WS-DAY
        + FUNCTION INTEGER((153 * (WS-JDN-M - 3) + 2) / 5)
        + 365 * WS-JDN-Y
        + FUNCTION INTEGER(WS-JDN-Y / 4)
        - FUNCTION INTEGER(WS-JDN-Y / 100)
        + FUNCTION INTEGER(WS-JDN-Y / 400)
        + 1721119.

*> ================================================================
*> CHECK-LEAP-YEAR - Check if WS-YEAR is a leap year
*> Output: WS-IS-LEAP (1=yes, 0=no)
*> ================================================================
CHECK-LEAP-YEAR.
    MOVE 0 TO WS-IS-LEAP
    IF FUNCTION MOD(WS-YEAR, 4) = 0
        IF FUNCTION MOD(WS-YEAR, 100) NOT = 0
            MOVE 1 TO WS-IS-LEAP
        ELSE
            IF FUNCTION MOD(WS-YEAR, 400) = 0
                MOVE 1 TO WS-IS-LEAP
            END-IF
        END-IF
    END-IF.

*> ================================================================
*> CALC-DAYS-IN-MONTH - Days in WS-BDAY-CUR-MONTH/WS-BDAY-CUR-YEAR
*> Output: WS-DAYS-IN-MONTH
*> ================================================================
CALC-DAYS-IN-MONTH.
    EVALUATE WS-BDAY-CUR-MONTH
        WHEN 1  MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 2
            MOVE WS-BDAY-CUR-YEAR TO WS-YEAR
            PERFORM CHECK-LEAP-YEAR
            IF WS-IS-LEAP = 1
                MOVE 29 TO WS-DAYS-IN-MONTH
            ELSE
                MOVE 28 TO WS-DAYS-IN-MONTH
            END-IF
        WHEN 3  MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 4  MOVE 30 TO WS-DAYS-IN-MONTH
        WHEN 5  MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 6  MOVE 30 TO WS-DAYS-IN-MONTH
        WHEN 7  MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 8  MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 9  MOVE 30 TO WS-DAYS-IN-MONTH
        WHEN 10 MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN 11 MOVE 30 TO WS-DAYS-IN-MONTH
        WHEN 12 MOVE 31 TO WS-DAYS-IN-MONTH
        WHEN OTHER
            MOVE 0 TO WS-DAYS-IN-MONTH
    END-EVALUATE.
