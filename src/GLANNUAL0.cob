IDENTIFICATION DIVISION.
PROGRAM-ID. GLANNUAL0.
*> ================================================================
*> GLANNUAL0 - General Ledger Annual Close
*> CLOS = Close income/expense accounts to Retained Earnings
*> ROLL = Roll balance sheet account accumulators and prior year bal
*> Reference: FFIEC Call Report & GAAP Year-End Ledger Close
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-SAVE-RE-BAL               PIC S9(15)V99.
01  WS-SAVE-RE-YTD               PIC S9(15)V99.

LINKAGE SECTION.
01  LS-FUNCTION                   PIC X(4).
COPY CPYGL.
01  RE-RECORD.
    05  RE-KEY.
        10  RE-ACCOUNT-NUM         PIC 9(10).
        10  RE-COST-CENTER         PIC 9(4).
    05  RE-CLASSIFICATION.
        10  RE-ACCT-TYPE           PIC X(1).
        10  RE-ACCT-SUBTYPE        PIC X(4).
        10  RE-ACCT-NAME           PIC X(40).
        10  RE-NORMAL-BALANCE      PIC X(1).
        10  RE-CALL-RPT-LINE       PIC X(6).
    05  RE-BALANCES.
        10  RE-CURRENT-BAL         PIC S9(15)V99.
        10  RE-MTD-DEBITS          PIC S9(15)V99.
        10  RE-MTD-CREDITS         PIC S9(15)V99.
        10  RE-YTD-DEBITS          PIC S9(15)V99.
        10  RE-YTD-CREDITS         PIC S9(15)V99.
        10  RE-PRIOR-MONTH-BAL     PIC S9(15)V99.
        10  RE-PRIOR-YEAR-BAL      PIC S9(15)V99.
        10  RE-BUDGET-BAL          PIC S9(15)V99.
    05  RE-CONTROL-FIELDS.
        10  RE-STATUS              PIC X(1).
        10  RE-AUTO-POST           PIC X(1).
        10  RE-RECONCILE-TYPE      PIC X(1).
        10  RE-LAST-POST-DATE      PIC 9(8).
        10  RE-LAST-RECON-DATE     PIC 9(8).
01  LS-ANNUAL-RESULT.
    05  LS-ANN-RESULT-CODE         PIC X(5).
    05  LS-ANN-RESULT-MSG          PIC X(50).
    05  LS-ANN-TRANSFER-AMT        PIC S9(15)V99.

PROCEDURE DIVISION USING LS-FUNCTION
                         GL-RECORD
                         RE-RECORD
                         LS-ANNUAL-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-ANN-RESULT-CODE
    MOVE SPACES TO LS-ANN-RESULT-MSG
    MOVE ZERO TO LS-ANN-TRANSFER-AMT

    EVALUATE LS-FUNCTION
        WHEN "CLOS"
            PERFORM CLOSE-GL-ACCOUNT
        WHEN "ROLL"
            PERFORM ROLL-GL-ACCOUNT
        WHEN OTHER
            MOVE "E0001" TO LS-ANN-RESULT-CODE
            MOVE "Invalid function" TO LS-ANN-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ----------------------------------------------------------------
*> CLOSE-GL-ACCOUNT - Close account to Retained Earnings
*> Income (I) & Expense (X) balances are transferred to Retained
*> Earnings and zeroed. Permanent accounts (A, L, E) are rolled.
*> ----------------------------------------------------------------
CLOSE-GL-ACCOUNT.
    IF GL-STATUS = "I"
        MOVE "E0061" TO LS-ANN-RESULT-CODE
        MOVE "GL account inactive" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF
    IF GL-STATUS = "F"
        MOVE "E0062" TO LS-ANN-RESULT-CODE
        MOVE "GL account frozen" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    EVALUATE GL-ACCT-TYPE
        WHEN "I"
            PERFORM CLOSE-INCOME-ACCOUNT
        WHEN "X"
            PERFORM CLOSE-EXPENSE-ACCOUNT
        WHEN "A"
        WHEN "L"
        WHEN "E"
            PERFORM ROLL-GL-ACCOUNT
        WHEN OTHER
            MOVE "E0002" TO LS-ANN-RESULT-CODE
            MOVE "Invalid GL account type" TO LS-ANN-RESULT-MSG
    END-EVALUATE.

*> ----------------------------------------------------------------
*> CLOSE-INCOME-ACCOUNT - Close Income account (Credit-normal)
*> Transfers credit balance to Retained Earnings (Credit RE)
*> ----------------------------------------------------------------
CLOSE-INCOME-ACCOUNT.
    IF RE-STATUS = "I"
        MOVE "E0061" TO LS-ANN-RESULT-CODE
        MOVE "Retained earnings account inactive" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF
    IF RE-STATUS = "F"
        MOVE "E0062" TO LS-ANN-RESULT-CODE
        MOVE "Retained earnings account frozen" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE GL-CURRENT-BAL TO LS-ANN-TRANSFER-AMT

    *> Credit Retained Earnings with income
    MOVE RE-CURRENT-BAL TO WS-SAVE-RE-BAL
    ADD GL-CURRENT-BAL TO RE-CURRENT-BAL
        ON SIZE ERROR
            MOVE "E0040" TO LS-ANN-RESULT-CODE
            MOVE "Arithmetic overflow on Retained Earnings"
                TO LS-ANN-RESULT-MSG
            EXIT PARAGRAPH
    END-ADD

    MOVE RE-YTD-CREDITS TO WS-SAVE-RE-YTD
    ADD GL-CURRENT-BAL TO RE-YTD-CREDITS
        ON SIZE ERROR
            MOVE WS-SAVE-RE-BAL TO RE-CURRENT-BAL
            MOVE "E0040" TO LS-ANN-RESULT-CODE
            MOVE "Arithmetic overflow on RE YTD credits"
                TO LS-ANN-RESULT-MSG
            EXIT PARAGRAPH
    END-ADD

    *> Archive ending balance to prior year and zero current balance
    MOVE GL-CURRENT-BAL TO GL-PRIOR-YEAR-BAL
    MOVE ZERO TO GL-CURRENT-BAL
    MOVE ZERO TO GL-MTD-DEBITS
    MOVE ZERO TO GL-MTD-CREDITS
    MOVE ZERO TO GL-YTD-DEBITS
    MOVE ZERO TO GL-YTD-CREDITS

    MOVE "E0000" TO LS-ANN-RESULT-CODE
    MOVE "Income closed to Retained Earnings" TO LS-ANN-RESULT-MSG.

*> ----------------------------------------------------------------
*> CLOSE-EXPENSE-ACCOUNT - Close Expense account (Debit-normal)
*> Transfers debit balance to Retained Earnings (Debit RE)
*> ----------------------------------------------------------------
CLOSE-EXPENSE-ACCOUNT.
    IF RE-STATUS = "I"
        MOVE "E0061" TO LS-ANN-RESULT-CODE
        MOVE "Retained earnings account inactive" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF
    IF RE-STATUS = "F"
        MOVE "E0062" TO LS-ANN-RESULT-CODE
        MOVE "Retained earnings account frozen" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE GL-CURRENT-BAL TO LS-ANN-TRANSFER-AMT

    *> Debit Retained Earnings with expense (reduces RE credit bal)
    MOVE RE-CURRENT-BAL TO WS-SAVE-RE-BAL
    SUBTRACT GL-CURRENT-BAL FROM RE-CURRENT-BAL
        ON SIZE ERROR
            MOVE "E0040" TO LS-ANN-RESULT-CODE
            MOVE "Arithmetic overflow on Retained Earnings"
                TO LS-ANN-RESULT-MSG
            EXIT PARAGRAPH
    END-SUBTRACT

    MOVE RE-YTD-DEBITS TO WS-SAVE-RE-YTD
    ADD GL-CURRENT-BAL TO RE-YTD-DEBITS
        ON SIZE ERROR
            MOVE WS-SAVE-RE-BAL TO RE-CURRENT-BAL
            MOVE "E0040" TO LS-ANN-RESULT-CODE
            MOVE "Arithmetic overflow on RE YTD debits"
                TO LS-ANN-RESULT-MSG
            EXIT PARAGRAPH
    END-ADD

    *> Archive ending balance to prior year and zero current balance
    MOVE GL-CURRENT-BAL TO GL-PRIOR-YEAR-BAL
    MOVE ZERO TO GL-CURRENT-BAL
    MOVE ZERO TO GL-MTD-DEBITS
    MOVE ZERO TO GL-MTD-CREDITS
    MOVE ZERO TO GL-YTD-DEBITS
    MOVE ZERO TO GL-YTD-CREDITS

    MOVE "E0000" TO LS-ANN-RESULT-CODE
    MOVE "Expense closed to Retained Earnings" TO LS-ANN-RESULT-MSG.

*> ----------------------------------------------------------------
*> ROLL-GL-ACCOUNT - Roll balance sheet account to prior year
*> Preserves current balance; rolls accumulators
*> ----------------------------------------------------------------
ROLL-GL-ACCOUNT.
    IF GL-STATUS = "I"
        MOVE "E0061" TO LS-ANN-RESULT-CODE
        MOVE "GL account inactive" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF
    IF GL-STATUS = "F"
        MOVE "E0062" TO LS-ANN-RESULT-CODE
        MOVE "GL account frozen" TO LS-ANN-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE GL-CURRENT-BAL TO GL-PRIOR-YEAR-BAL
    MOVE ZERO TO GL-MTD-DEBITS
    MOVE ZERO TO GL-MTD-CREDITS
    MOVE ZERO TO GL-YTD-DEBITS
    MOVE ZERO TO GL-YTD-CREDITS
    MOVE ZERO TO LS-ANN-TRANSFER-AMT

    MOVE "E0000" TO LS-ANN-RESULT-CODE
    MOVE "Balance sheet account rolled" TO LS-ANN-RESULT-MSG.
