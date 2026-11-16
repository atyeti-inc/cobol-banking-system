IDENTIFICATION DIVISION.
PROGRAM-ID. TAXRPT0.
*> ================================================================
*> TAXRPT0 - Year-End Tax Reporting Engine
*> Generates IRS Form 1099-INT extract records for deposit accounts
*> with interest >= $10.00, and Form 1098 records for loans with
*> interest paid >= $600.00.
*> Reference: 26 U.S. Code Sections 6049 (1099-INT) and 6050H (1098)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CONSTANTS.
    05  WS-DEFAULT-PAYER-TIN     PIC 9(9) VALUE 123456789.
    05  WS-DEFAULT-PAYER-NAME    PIC X(40) VALUE "VAULT CORE BANKING SYSTEM".
    05  WS-1099-INT-THRESHOLD    PIC S9(5)V99 VALUE +10.00.
    05  WS-1098-MORT-THRESHOLD   PIC S9(5)V99 VALUE +600.00.

LINKAGE SECTION.
01  LS-TAX-YEAR                  PIC 9(4).
COPY CPYACCT.
COPY CPYCIF.
COPY CPYTAX.
01  LS-TAX-RESULT.
    05  LS-TAX-RESULT-CODE       PIC X(5).
    05  LS-TAX-RESULT-MSG        PIC X(50).
    05  LS-TAX-FORM-TYPE         PIC X(4).

PROCEDURE DIVISION USING LS-TAX-YEAR
                         ACCT-RECORD
                         CIF-RECORD
                         TAX-1099-RECORD
                         TAX-1098-RECORD
                         LS-TAX-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-TAX-RESULT-CODE
    MOVE SPACES TO LS-TAX-RESULT-MSG
    MOVE "NONE" TO LS-TAX-FORM-TYPE

    INITIALIZE TAX-1099-RECORD
    INITIALIZE TAX-1098-RECORD
    MOVE "N" TO TAX-1099-QUALIFIED
    MOVE "S" TO TAX-1099-FILING-STATUS
    MOVE "N" TO TAX-1098-QUALIFIED
    MOVE "S" TO TAX-1098-FILING-STATUS

    *> Validate tax year
    IF LS-TAX-YEAR < 1900 OR LS-TAX-YEAR > 2099
        MOVE "E0002" TO LS-TAX-RESULT-CODE
        MOVE "Invalid tax year" TO LS-TAX-RESULT-MSG
        GOBACK
    END-IF

    *> Validate account number
    IF ACCT-NUMBER = ZERO
        MOVE "E0010" TO LS-TAX-RESULT-CODE
        MOVE "Account not found" TO LS-TAX-RESULT-MSG
        GOBACK
    END-IF

    EVALUATE ACCT-TYPE
        WHEN "D"
            PERFORM PROCESS-1099-INT
        WHEN "L"
            PERFORM PROCESS-1098
        WHEN OTHER
            MOVE "NONE" TO LS-TAX-FORM-TYPE
            MOVE "E0000" TO LS-TAX-RESULT-CODE
            MOVE "Account type not subject to 1099/1098 reporting"
                TO LS-TAX-RESULT-MSG
    END-EVALUATE

    GOBACK.

*> ----------------------------------------------------------------
*> Process deposit account for Form 1099-INT reporting
*> ----------------------------------------------------------------
PROCESS-1099-INT.
    MOVE LS-TAX-YEAR TO TAX-1099-TAX-YEAR
    MOVE ACCT-NUMBER TO TAX-1099-ACCT-NUMBER
    MOVE ACCT-PRIMARY-CIF TO TAX-1099-CIF-ID

    *> Payer information
    MOVE WS-DEFAULT-PAYER-TIN TO TAX-1099-PAYER-TIN
    MOVE WS-DEFAULT-PAYER-NAME TO TAX-1099-PAYER-NAME

    *> Recipient information
    MOVE CIF-SSN-TIN TO TAX-1099-RECIP-TIN
    IF CIF-SSN-TYPE NOT = SPACES
        MOVE CIF-SSN-TYPE TO TAX-1099-RECIP-TIN-TYPE
    ELSE
        MOVE "S" TO TAX-1099-RECIP-TIN-TYPE
    END-IF

    IF CIF-NAME-LAST NOT = SPACES
        STRING FUNCTION TRIM(CIF-NAME-FIRST) " "
               FUNCTION TRIM(CIF-NAME-LAST)
            DELIMITED BY SIZE INTO TAX-1099-RECIP-NAME
    END-IF

    IF CIF-ADDR-LINE1 NOT = SPACES
        STRING FUNCTION TRIM(CIF-ADDR-LINE1) ", "
               FUNCTION TRIM(CIF-CITY) " "
               CIF-STATE " " CIF-ZIP
            DELIMITED BY SIZE INTO TAX-1099-RECIP-ADDR
    END-IF

    MOVE ACCT-YTD-INT-PAID TO TAX-1099-INT-INCOME
    MOVE ACCT-CD-EARLY-WD-PEN TO TAX-1099-EARLY-WD-PEN
    MOVE ZERO TO TAX-1099-FED-TAX-WH

    *> Threshold check: $10.00 minimum for 1099-INT
    IF ACCT-YTD-INT-PAID >= WS-1099-INT-THRESHOLD
        MOVE "Y" TO TAX-1099-QUALIFIED
        MOVE "G" TO TAX-1099-FILING-STATUS
        MOVE "1099" TO LS-TAX-FORM-TYPE
        MOVE "E0000" TO LS-TAX-RESULT-CODE
        MOVE "1099-INT generated successfully"
            TO LS-TAX-RESULT-MSG
    ELSE
        MOVE "N" TO TAX-1099-QUALIFIED
        MOVE "S" TO TAX-1099-FILING-STATUS
        MOVE "NONE" TO LS-TAX-FORM-TYPE
        MOVE "E0000" TO LS-TAX-RESULT-CODE
        MOVE "Interest below 1099-INT threshold ($10.00)"
            TO LS-TAX-RESULT-MSG
    END-IF.

*> ----------------------------------------------------------------
*> Process loan account for Form 1098 reporting
*> ----------------------------------------------------------------
PROCESS-1098.
    MOVE LS-TAX-YEAR TO TAX-1098-TAX-YEAR
    MOVE ACCT-NUMBER TO TAX-1098-ACCT-NUMBER
    MOVE ACCT-PRIMARY-CIF TO TAX-1098-CIF-ID

    *> Lender information
    MOVE WS-DEFAULT-PAYER-TIN TO TAX-1098-LENDER-TIN
    MOVE WS-DEFAULT-PAYER-NAME TO TAX-1098-LENDER-NAME

    *> Borrower information
    MOVE CIF-SSN-TIN TO TAX-1098-BORR-TIN
    IF CIF-SSN-TYPE NOT = SPACES
        MOVE CIF-SSN-TYPE TO TAX-1098-BORR-TIN-TYPE
    ELSE
        MOVE "S" TO TAX-1098-BORR-TIN-TYPE
    END-IF

    IF CIF-NAME-LAST NOT = SPACES
        STRING FUNCTION TRIM(CIF-NAME-FIRST) " "
               FUNCTION TRIM(CIF-NAME-LAST)
            DELIMITED BY SIZE INTO TAX-1098-BORR-NAME
    END-IF

    IF CIF-ADDR-LINE1 NOT = SPACES
        STRING FUNCTION TRIM(CIF-ADDR-LINE1) ", "
               FUNCTION TRIM(CIF-CITY) " "
               CIF-STATE " " CIF-ZIP
            DELIMITED BY SIZE INTO TAX-1098-BORR-ADDR
    END-IF

    MOVE ACCT-YTD-INT-PAID TO TAX-1098-MORT-INT-RECV
    MOVE ACCT-LEDGER-BAL TO TAX-1098-OUTSTANDING-PRIN
    MOVE ZERO TO TAX-1098-OVERPAY-REFUND

    *> Threshold check: $600.00 minimum for 1098
    IF ACCT-YTD-INT-PAID >= WS-1098-MORT-THRESHOLD
        MOVE "Y" TO TAX-1098-QUALIFIED
        MOVE "G" TO TAX-1098-FILING-STATUS
        MOVE "1098" TO LS-TAX-FORM-TYPE
        MOVE "E0000" TO LS-TAX-RESULT-CODE
        MOVE "1098 generated successfully"
            TO LS-TAX-RESULT-MSG
    ELSE
        MOVE "N" TO TAX-1098-QUALIFIED
        MOVE "S" TO TAX-1098-FILING-STATUS
        MOVE "NONE" TO LS-TAX-FORM-TYPE
        MOVE "E0000" TO LS-TAX-RESULT-CODE
        MOVE "Mortgage interest below 1098 threshold ($600.00)"
            TO LS-TAX-RESULT-MSG
    END-IF.
