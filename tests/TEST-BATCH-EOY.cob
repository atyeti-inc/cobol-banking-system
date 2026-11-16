IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BATCH-EOY.
*> ================================================================
*> TEST-BATCH-EOY - Integration and Unit Tests for EOY Annual Close
*> Tests: TAXRPT0 (1099-INT / 1098), GLANNUAL0 (GL close to Retained
*>        Earnings), and EOYPROC0 (Batch Orchestration) (21 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> EOY batch areas
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  WS-BATCH-RESULT.
    05  WS-BATCH-RESULT-CODE  PIC X(5).
    05  WS-BATCH-RESULT-MSG   PIC X(50).

*> Tax reporting test areas
01  WS-TAX-YEAR            PIC 9(4).
COPY CPYCIF.
COPY CPYTAX.
01  WS-TAX-RESULT.
    05  WS-TAX-RESULT-CODE    PIC X(5).
    05  WS-TAX-RESULT-MSG     PIC X(50).
    05  WS-TAX-FORM-TYPE      PIC X(4).

*> GL annual close test areas
01  WS-GL-FUNCTION         PIC X(4).
COPY CPYGL.
01  WS-RE-RECORD.
    05  WS-RE-KEY.
        10  WS-RE-ACCOUNT-NUM     PIC 9(10).
        10  WS-RE-COST-CENTER     PIC 9(4).
    05  WS-RE-CLASSIFICATION.
        10  WS-RE-ACCT-TYPE       PIC X(1).
        10  WS-RE-ACCT-SUBTYPE    PIC X(4).
        10  WS-RE-ACCT-NAME       PIC X(40).
        10  WS-RE-NORMAL-BALANCE  PIC X(1).
        10  WS-RE-CALL-RPT-LINE   PIC X(6).
    05  WS-RE-BALANCES.
        10  WS-RE-CURRENT-BAL     PIC S9(15)V99.
        10  WS-RE-MTD-DEBITS      PIC S9(15)V99.
        10  WS-RE-MTD-CREDITS     PIC S9(15)V99.
        10  WS-RE-YTD-DEBITS      PIC S9(15)V99.
        10  WS-RE-YTD-CREDITS     PIC S9(15)V99.
        10  WS-RE-PRIOR-MONTH-BAL PIC S9(15)V99.
        10  WS-RE-PRIOR-YEAR-BAL  PIC S9(15)V99.
        10  WS-RE-BUDGET-BAL      PIC S9(15)V99.
    05  WS-RE-CONTROL-FIELDS.
        10  WS-RE-STATUS          PIC X(1).
        10  WS-RE-AUTO-POST       PIC X(1).
        10  WS-RE-RECONCILE-TYPE  PIC X(1).
        10  WS-RE-LAST-POST-DATE  PIC 9(8).
        10  WS-RE-LAST-RECON-DATE PIC 9(8).
01  WS-ANNUAL-RESULT.
    05  WS-ANN-RESULT-CODE    PIC X(5).
    05  WS-ANN-RESULT-MSG     PIC X(50).
    05  WS-ANN-TRANSFER-AMT   PIC S9(15)V99.

01  WS-SAVED-LEDGER-BAL    PIC S9(13)V99.
01  WS-SAVED-AVAIL-BAL     PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: BATCH-EOY Integration & Unit".
    DISPLAY "========================================".

    PERFORM TEST-EY-001
    PERFORM TEST-EY-002
    PERFORM TEST-EY-003
    PERFORM TEST-EY-004
    PERFORM TEST-EY-005
    PERFORM TEST-EY-006
    PERFORM TEST-EY-007
    PERFORM TEST-EY-008
    PERFORM TEST-EY-009
    PERFORM TEST-EY-010
    PERFORM TEST-EY-011
    PERFORM TEST-EY-012
    PERFORM TEST-EY-013
    PERFORM TEST-EY-014
    PERFORM TEST-EY-015
    PERFORM TEST-EY-016
    PERFORM TEST-EY-017
    PERFORM TEST-EY-018
    PERFORM TEST-EY-019
    PERFORM TEST-EY-020
    PERFORM TEST-EY-021

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helpers
*> ---------------------------------------------------------------
SETUP-DEPOSIT-ACCT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA2" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 15000.00 TO ACCT-LEDGER-BAL
    MOVE 15000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE 120.50 TO ACCT-YTD-INT-PAID
    MOVE 120.500000 TO ACCT-YTD-INT-EARNED
    MOVE 36.00 TO ACCT-YTD-FEES-CHARGED
    MOVE 12.00 TO ACCT-YTD-FEES-WAIVED
    MOVE 2 TO ACCT-NSF-COUNT-YTD.

SETUP-LOAN-ACCT.
    INITIALIZE ACCT-RECORD
    MOVE 000087654321 TO ACCT-NUMBER
    MOVE 1 TO ACCT-CHECK-DIGIT
    MOVE 1000000002 TO ACCT-PRIMARY-CIF
    MOVE "LN01" TO ACCT-PRODUCT-CODE
    MOVE "L" TO ACCT-TYPE
    MOVE "LN" TO ACCT-SUB-TYPE
    MOVE 250000.00 TO ACCT-LEDGER-BAL
    MOVE 250000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE 8450.00 TO ACCT-YTD-INT-PAID.

SETUP-CIF.
    INITIALIZE CIF-RECORD
    MOVE 1000000001 TO CIF-CUST-ID
    MOVE "SMITH" TO CIF-NAME-LAST
    MOVE "JOHN" TO CIF-NAME-FIRST
    MOVE 123456789 TO CIF-SSN-TIN
    MOVE "S" TO CIF-SSN-TYPE
    MOVE "123 MAIN ST" TO CIF-ADDR-LINE1
    MOVE "NEW YORK" TO CIF-CITY
    MOVE "NY" TO CIF-STATE
    MOVE 100010000 TO CIF-ZIP.

SETUP-RE-RECORD.
    INITIALIZE WS-RE-RECORD
    MOVE 0000006030 TO WS-RE-ACCOUNT-NUM
    MOVE 0001 TO WS-RE-COST-CENTER
    MOVE "E" TO WS-RE-ACCT-TYPE
    MOVE "RE" TO WS-RE-ACCT-SUBTYPE
    MOVE "Retained Earnings" TO WS-RE-ACCT-NAME
    MOVE "C" TO WS-RE-NORMAL-BALANCE
    MOVE 500000.00 TO WS-RE-CURRENT-BAL
    MOVE "A" TO WS-RE-STATUS.

*> ---------------------------------------------------------------
*> EY-001: 1099-INT generated when interest paid >= $10.00
*> ---------------------------------------------------------------
TEST-EY-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-001: 1099-INT qualified when int >= $10" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0000"
        AND TAX-1099-QUALIFIED = "Y"
        AND TAX-1099-FILING-STATUS = "G"
        AND TAX-1099-INT-INCOME = 120.50
        AND WS-TAX-FORM-TYPE = "1099"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-TAX-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-002: 1099-INT suppressed when interest paid < $10.00
*> ---------------------------------------------------------------
TEST-EY-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-002: 1099-INT suppressed when int < $10" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE 9.99 TO ACCT-YTD-INT-PAID
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0000"
        AND TAX-1099-QUALIFIED = "N"
        AND TAX-1099-FILING-STATUS = "S"
        AND WS-TAX-FORM-TYPE = "NONE"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " qualified=" TAX-1099-QUALIFIED
    END-IF.

*> ---------------------------------------------------------------
*> EY-003: Form 1098 qualified when loan interest >= $600.00
*> ---------------------------------------------------------------
TEST-EY-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-003: 1098 qualified when loan int >= $600" TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCT
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0000"
        AND TAX-1098-QUALIFIED = "Y"
        AND TAX-1098-FILING-STATUS = "G"
        AND TAX-1098-MORT-INT-RECV = 8450.00
        AND TAX-1098-OUTSTANDING-PRIN = 250000.00
        AND WS-TAX-FORM-TYPE = "1098"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-TAX-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-004: Form 1098 suppressed when loan interest < $600.00
*> ---------------------------------------------------------------
TEST-EY-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-004: 1098 suppressed when loan int < $600" TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCT
    MOVE 599.99 TO ACCT-YTD-INT-PAID
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0000"
        AND TAX-1098-QUALIFIED = "N"
        AND TAX-1098-FILING-STATUS = "S"
        AND WS-TAX-FORM-TYPE = "NONE"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " qual=" TAX-1098-QUALIFIED
    END-IF.

*> ---------------------------------------------------------------
*> EY-005: TAXRPT0 formats recipient name & TIN from CIF
*> ---------------------------------------------------------------
TEST-EY-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-005: Recipient name and TIN formatted from CIF"
        TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF TAX-1099-RECIP-TIN = 123456789
        AND TAX-1099-RECIP-TIN-TYPE = "S"
        AND TAX-1099-RECIP-NAME = "JOHN SMITH"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " name=" TAX-1099-RECIP-NAME
    END-IF.

*> ---------------------------------------------------------------
*> EY-006: TAXRPT0 invalid tax year returns E0002
*> ---------------------------------------------------------------
TEST-EY-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-006: Invalid tax year returns E0002" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    PERFORM SETUP-CIF
    MOVE 1899 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-TAX-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-007: TAXRPT0 zero account number returns E0010
*> ---------------------------------------------------------------
TEST-EY-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-007: Zero account number returns E0010" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE ZERO TO ACCT-NUMBER
    PERFORM SETUP-CIF
    MOVE 2026 TO WS-TAX-YEAR
    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT
    IF WS-TAX-RESULT-CODE = "E0010"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-TAX-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-008: GLANNUAL0 closes Income to Retained Earnings
*> ---------------------------------------------------------------
TEST-EY-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-008: Close Income to Retained Earnings" TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    INITIALIZE GL-RECORD
    MOVE 0000007500 TO GL-ACCOUNT-NUM
    MOVE "I" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 75000.00 TO GL-CURRENT-BAL
    MOVE "A" TO GL-STATUS
    MOVE "CLOS" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT
    IF WS-ANN-RESULT-CODE = "E0000"
        AND GL-CURRENT-BAL = ZERO
        AND GL-PRIOR-YEAR-BAL = 75000.00
        AND WS-RE-CURRENT-BAL = 575000.00
        AND WS-ANN-TRANSFER-AMT = 75000.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " re_bal=" WS-RE-CURRENT-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EY-009: GLANNUAL0 closes Expense to Retained Earnings
*> ---------------------------------------------------------------
TEST-EY-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-009: Close Expense to Retained Earnings" TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    INITIALIZE GL-RECORD
    MOVE 0000008010 TO GL-ACCOUNT-NUM
    MOVE "X" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 25000.00 TO GL-CURRENT-BAL
    MOVE "A" TO GL-STATUS
    MOVE "CLOS" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT
    IF WS-ANN-RESULT-CODE = "E0000"
        AND GL-CURRENT-BAL = ZERO
        AND GL-PRIOR-YEAR-BAL = 25000.00
        AND WS-RE-CURRENT-BAL = 475000.00
        AND WS-ANN-TRANSFER-AMT = 25000.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " re_bal=" WS-RE-CURRENT-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EY-010: GLANNUAL0 rolls balance sheet account preserving balance
*> ---------------------------------------------------------------
TEST-EY-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-010: Roll Asset account preserves balance" TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    INITIALIZE GL-RECORD
    MOVE 0000001010 TO GL-ACCOUNT-NUM
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 1000000.00 TO GL-CURRENT-BAL
    MOVE 50000.00 TO GL-YTD-DEBITS
    MOVE 20000.00 TO GL-YTD-CREDITS
    MOVE "A" TO GL-STATUS
    MOVE "ROLL" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT
    IF WS-ANN-RESULT-CODE = "E0000"
        AND GL-CURRENT-BAL = 1000000.00
        AND GL-PRIOR-YEAR-BAL = 1000000.00
        AND GL-YTD-DEBITS = ZERO
        AND GL-YTD-CREDITS = ZERO
        AND WS-RE-CURRENT-BAL = 500000.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " bal=" GL-CURRENT-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EY-011: GLANNUAL0 inactive GL account returns E0061
*> ---------------------------------------------------------------
TEST-EY-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-011: Inactive GL account returns E0061" TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    INITIALIZE GL-RECORD
    MOVE "I" TO GL-STATUS
    MOVE "CLOS" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT
    IF WS-ANN-RESULT-CODE = "E0061"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-ANN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-012: GLANNUAL0 frozen GL account returns E0062
*> ---------------------------------------------------------------
TEST-EY-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-012: Frozen GL account returns E0062" TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    INITIALIZE GL-RECORD
    MOVE "F" TO GL-STATUS
    MOVE "CLOS" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT
    IF WS-ANN-RESULT-CODE = "E0062"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-ANN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-013: GL Trial Balance equality maintained after annual close
*> Pre-close:
*>   Cash (Asset, D): 150,000.00
*>   Deposits (Liability, C): 100,000.00
*>   Retained Earnings (Equity, C): 20,000.00
*>   Fee Income (Income, C): 40,000.00
*>   Interest Expense (Expense, D): 10,000.00
*>   Total DR: 160,000.00 = Total CR: 160,000.00
*> Post-close:
*>   Income and Expense closed to Retained Earnings
*>   Net Income: 40,000 - 10,000 = 30,000
*>   Retained Earnings becomes: 20,000 + 30,000 = 50,000
*>   Total DR: Cash (150,000) = Total CR: Deposits (100,000) + RE (50,000)
*> ---------------------------------------------------------------
TEST-EY-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-013: Trial Balance balanced after annual close"
        TO WS-TEST-NAME
    PERFORM SETUP-RE-RECORD
    MOVE 20000.00 TO WS-RE-CURRENT-BAL

    *> 1. Close Income
    INITIALIZE GL-RECORD
    MOVE 0000007500 TO GL-ACCOUNT-NUM
    MOVE "I" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 40000.00 TO GL-CURRENT-BAL
    MOVE "A" TO GL-STATUS
    MOVE "CLOS" TO WS-GL-FUNCTION
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT

    *> 2. Close Expense
    INITIALIZE GL-RECORD
    MOVE 0000008010 TO GL-ACCOUNT-NUM
    MOVE "X" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 10000.00 TO GL-CURRENT-BAL
    MOVE "A" TO GL-STATUS
    CALL "GLANNUAL0" USING WS-GL-FUNCTION
                           GL-RECORD
                           WS-RE-RECORD
                           WS-ANNUAL-RESULT

    *> Verify post-close balances:
    *> Cash: 150,000 (DR)
    *> Deposits (100,000) + Retained Earnings (50,000) = 150,000 (CR)
    IF WS-RE-CURRENT-BAL = 50000.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " re_bal=" WS-RE-CURRENT-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EY-014: EOYPROC0 completes successfully -> C, E0000
*> ---------------------------------------------------------------
TEST-EY-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-014: EOY batch completes -> status C, E0000"
        TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-STATUS = "C"
        AND BATCH-TYPE = "EOY"
        AND BATCH-ACCTS-PROCESSED = 1
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " status=" BATCH-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> EY-015: EOYPROC0 zeroes account YTD accumulators
*> ---------------------------------------------------------------
TEST-EY-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-015: Account YTD accumulators zeroed out" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-YTD-INT-PAID = ZERO
        AND ACCT-YTD-INT-EARNED = ZERO
        AND ACCT-YTD-FEES-CHARGED = ZERO
        AND ACCT-YTD-FEES-WAIVED = ZERO
        AND ACCT-NSF-COUNT-YTD = ZERO
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " ytd_paid=" ACCT-YTD-INT-PAID
    END-IF.

*> ---------------------------------------------------------------
*> EY-016: EOYPROC0 preserves ledger balance and available balance
*> ---------------------------------------------------------------
TEST-EY-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-016: Ledger & available balances preserved" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    MOVE ACCT-AVAIL-BAL TO WS-SAVED-AVAIL-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-LEDGER-BAL = WS-SAVED-LEDGER-BAL
        AND ACCT-AVAIL-BAL = WS-SAVED-AVAIL-BAL
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " bal=" ACCT-LEDGER-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EY-017: Qualified tax forms increment batch tax forms count
*> ---------------------------------------------------------------
TEST-EY-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-017: Batch tax forms count incremented" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE 150.00 TO ACCT-YTD-INT-PAID
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-CTR-FILED = 1
        AND BATCH-INT-PAID = 150.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " tax_count=" BATCH-CTR-FILED
    END-IF.

*> ---------------------------------------------------------------
*> EY-018: EOYPROC0 zero batch date returns E0002
*> ---------------------------------------------------------------
TEST-EY-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-018: Zero batch date returns E0002 and status F"
        TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE ZERO TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0002"
        AND BATCH-STATUS = "F"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-019: Frozen account processed for YTD reset without error
*> ---------------------------------------------------------------
TEST-EY-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-019: Frozen account YTD reset completes" TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE "F" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-STATUS = "C"
        AND ACCT-YTD-INT-PAID = ZERO
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EY-020: Closed account with zero interest skipped
*> ---------------------------------------------------------------
TEST-EY-020.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-020: Closed account with zero interest skipped"
        TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE "C" TO ACCT-STATUS
    MOVE ZERO TO ACCT-YTD-INT-PAID
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-ACCTS-PROCESSED = ZERO
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " accts=" BATCH-ACCTS-PROCESSED
    END-IF.

*> ---------------------------------------------------------------
*> EY-021: Closed account with positive interest generates 1099
*> ---------------------------------------------------------------
TEST-EY-021.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EY-021: Closed account with interest generates 1099"
        TO WS-TEST-NAME
    PERFORM SETUP-DEPOSIT-ACCT
    MOVE "C" TO ACCT-STATUS
    MOVE 75.00 TO ACCT-YTD-INT-PAID
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOYPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-ACCTS-PROCESSED = 1
        AND BATCH-CTR-FILED = 1
        AND ACCT-YTD-INT-PAID = ZERO
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " filed=" BATCH-CTR-FILED
    END-IF.
