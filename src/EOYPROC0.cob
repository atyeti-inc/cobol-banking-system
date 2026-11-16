IDENTIFICATION DIVISION.
PROGRAM-ID. EOYPROC0.
*> ================================================================
*> EOYPROC0 - End-of-Year Batch Processor
*> Coordinates tax reporting extraction (1099/1098), resets
*> account-level YTD accumulators, and logs audit records.
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.
COPY CPYCIF.
COPY CPYTAX.

*> Tax calculation areas
01  WS-TAX-YEAR               PIC 9(4).
01  WS-TAX-RESULT.
    05  WS-TAX-RESULT-CODE    PIC X(5).
    05  WS-TAX-RESULT-MSG     PIC X(50).
    05  WS-TAX-FORM-TYPE      PIC X(4).

*> Audit areas
01  WS-AUDIT-FUNCTION         PIC X(4).
COPY CPYAUDT.
01  WS-AUDIT-RESULT.
    05  WS-AUDT-RESULT-CODE   PIC X(5).
    05  WS-AUDT-RESULT-MSG    PIC X(50).

*> Batch accumulators
01  WS-ACCTS-PROCESSED        PIC 9(8) VALUE 0.
01  WS-ACCTS-ERRORS           PIC 9(8) VALUE 0.
01  WS-TAX-FORMS-COUNT        PIC 9(5) VALUE 0.
01  WS-TOTAL-INT-PAID         PIC S9(15)V99 VALUE 0.
01  WS-TOTAL-FEES             PIC S9(15)V99 VALUE 0.

LINKAGE SECTION.
01  LS-BATCH-DATE             PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  LS-BATCH-RESULT.
    05  LS-BATCH-RESULT-CODE  PIC X(5).
    05  LS-BATCH-RESULT-MSG   PIC X(50).

PROCEDURE DIVISION USING LS-BATCH-DATE
                         ACCT-RECORD
                         BATCH-RECORD
                         LS-BATCH-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-BATCH-RESULT-CODE
    MOVE SPACES TO LS-BATCH-RESULT-MSG
    MOVE 0 TO WS-ACCTS-PROCESSED
    MOVE 0 TO WS-ACCTS-ERRORS
    MOVE 0 TO WS-TAX-FORMS-COUNT
    MOVE 0 TO WS-TOTAL-INT-PAID
    MOVE 0 TO WS-TOTAL-FEES

    *> Validate batch date
    IF LS-BATCH-DATE = ZERO
        MOVE "E0002" TO LS-BATCH-RESULT-CODE
        MOVE "Batch date required" TO LS-BATCH-RESULT-MSG
        MOVE "F" TO BATCH-STATUS
        GOBACK
    END-IF

    *> Update batch control record
    MOVE LS-BATCH-DATE TO BATCH-DATE
    MOVE "EOY" TO BATCH-TYPE
    MOVE "S" TO BATCH-STATUS

    *> Extract 4-digit tax year from batch date YYYYMMDD
    COMPUTE WS-TAX-YEAR = FUNCTION INTEGER-PART(LS-BATCH-DATE / 10000)

    *> Process provided account
    *> Active, Dormant, Frozen, Restricted, and closed accounts with interest
    IF ACCT-STATUS = "A" OR ACCT-STATUS = "D"
        OR ACCT-STATUS = "F" OR ACCT-STATUS = "R"
        OR (ACCT-STATUS = "C" AND ACCT-YTD-INT-PAID > ZERO)
        PERFORM PROCESS-EOY-ACCOUNT
    END-IF

    *> Write batch summary
    MOVE WS-ACCTS-PROCESSED TO BATCH-ACCTS-PROCESSED
    MOVE WS-ACCTS-ERRORS TO BATCH-ACCTS-ERRORS
    MOVE WS-TOTAL-INT-PAID TO BATCH-INT-PAID
    MOVE WS-TOTAL-FEES TO BATCH-FEES-ASSESSED
    MOVE WS-TAX-FORMS-COUNT TO BATCH-CTR-FILED

    IF WS-ACCTS-ERRORS = 0
        MOVE "C" TO BATCH-STATUS
        MOVE "E0000" TO LS-BATCH-RESULT-CODE
        MOVE "EOY batch completed successfully"
            TO LS-BATCH-RESULT-MSG
    ELSE
        MOVE "P" TO BATCH-STATUS
        MOVE "E0003" TO LS-BATCH-RESULT-CODE
        MOVE "EOY batch completed with errors"
            TO LS-BATCH-RESULT-MSG
    END-IF
    GOBACK.

*> ----------------------------------------------------------------
*> Process a single account for EOY
*> ----------------------------------------------------------------
PROCESS-EOY-ACCOUNT.
    ADD 1 TO WS-ACCTS-PROCESSED

    *> Step 1: Evaluate tax reporting via TAXRPT0
    PERFORM EOY-EVALUATE-TAX

    *> Step 2: Accumulate totals before reset
    ADD ACCT-YTD-INT-PAID TO WS-TOTAL-INT-PAID
        ON SIZE ERROR
            ADD 1 TO WS-ACCTS-ERRORS
    END-ADD

    ADD ACCT-YTD-FEES-CHARGED TO WS-TOTAL-FEES
        ON SIZE ERROR
            ADD 1 TO WS-ACCTS-ERRORS
    END-ADD

    *> Step 3: Reset YTD accumulators
    PERFORM EOY-RESET-YTD

    *> Step 4: Write audit log
    PERFORM EOY-LOG-AUDIT.

*> ----------------------------------------------------------------
*> Evaluate account for 1099-INT / 1098 generation
*> ----------------------------------------------------------------
EOY-EVALUATE-TAX.
    INITIALIZE CIF-RECORD
    MOVE ACCT-PRIMARY-CIF TO CIF-CUST-ID

    INITIALIZE TAX-1099-RECORD
    INITIALIZE TAX-1098-RECORD
    INITIALIZE WS-TAX-RESULT

    CALL "TAXRPT0" USING WS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         WS-TAX-RESULT

    IF WS-TAX-RESULT-CODE = "E0000"
        IF TAX-1099-QUALIFIED = "Y" OR TAX-1098-QUALIFIED = "Y"
            ADD 1 TO WS-TAX-FORMS-COUNT
        END-IF
    ELSE
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.

*> ----------------------------------------------------------------
*> Reset YTD accumulators
*> ----------------------------------------------------------------
EOY-RESET-YTD.
    MOVE ZERO TO ACCT-NSF-COUNT-YTD
    MOVE ZERO TO ACCT-YTD-FEES-CHARGED
    MOVE ZERO TO ACCT-YTD-FEES-WAIVED
    MOVE ZERO TO ACCT-YTD-INT-EARNED
    MOVE ZERO TO ACCT-YTD-INT-PAID.

*> ----------------------------------------------------------------
*> Write audit trail record for EOY processing
*> ----------------------------------------------------------------
EOY-LOG-AUDIT.
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "SYSTEM" TO AUDIT-USER-ID
    MOVE "EOYPROC0" TO AUDIT-PROGRAM-ID
    MOVE "EOY " TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE ACCT-NUMBER TO AUDIT-ENTITY-KEY
    MOVE "YTD-RESET" TO AUDIT-FIELD-NAME
    MOVE "EOY annual close complete" TO AUDIT-DESCRIPTION

    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION
                          AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.
