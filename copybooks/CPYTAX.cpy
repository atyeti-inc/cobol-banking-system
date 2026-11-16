*> ================================================================
*> CPYTAX.cpy - IRS Tax Reporting Record Layouts
*> Used by: TAXRPT0, EOYPROC0 for year-end tax reporting
*> Supports IRS Form 1099-INT and Form 1098
*> ================================================================
01  TAX-1099-RECORD.
    05  TAX-1099-KEY.
        10  TAX-1099-TAX-YEAR         PIC 9(4).
        10  TAX-1099-ACCT-NUMBER      PIC 9(12).
        10  TAX-1099-CIF-ID           PIC 9(10).
    05  TAX-1099-PAYER-INFO.
        10  TAX-1099-PAYER-TIN        PIC 9(9).
        10  TAX-1099-PAYER-NAME       PIC X(40).
    05  TAX-1099-RECIPIENT-INFO.
        10  TAX-1099-RECIP-TIN        PIC 9(9).
        10  TAX-1099-RECIP-TIN-TYPE   PIC X(1).
        *> S = SSN, E = EIN, I = ITIN
        10  TAX-1099-RECIP-NAME       PIC X(50).
        10  TAX-1099-RECIP-ADDR       PIC X(80).
    05  TAX-1099-AMOUNTS.
        10  TAX-1099-INT-INCOME       PIC S9(11)V99.
        *> Box 1: Interest income
        10  TAX-1099-EARLY-WD-PEN     PIC S9(9)V99.
        *> Box 2: Early withdrawal penalty
        10  TAX-1099-FED-TAX-WH       PIC S9(9)V99.
        *> Box 4: Federal income tax withheld
    05  TAX-1099-STATUS.
        10  TAX-1099-QUALIFIED        PIC X(1).
        *> Y = Qualified (>= $10.00), N = Below threshold
        10  TAX-1099-FILING-STATUS    PIC X(1).
        *> G = Generated, S = Skipped, E = Error

01  TAX-1098-RECORD.
    05  TAX-1098-KEY.
        10  TAX-1098-TAX-YEAR         PIC 9(4).
        10  TAX-1098-ACCT-NUMBER      PIC 9(12).
        10  TAX-1098-CIF-ID           PIC 9(10).
    05  TAX-1098-LENDER-INFO.
        10  TAX-1098-LENDER-TIN       PIC 9(9).
        10  TAX-1098-LENDER-NAME      PIC X(40).
    05  TAX-1098-BORROWER-INFO.
        10  TAX-1098-BORR-TIN         PIC 9(9).
        10  TAX-1098-BORR-TIN-TYPE    PIC X(1).
        *> S = SSN, E = EIN, I = ITIN
        10  TAX-1098-BORR-NAME        PIC X(50).
        10  TAX-1098-BORR-ADDR        PIC X(80).
    05  TAX-1098-AMOUNTS.
        10  TAX-1098-MORT-INT-RECV    PIC S9(11)V99.
        *> Box 1: Mortgage/Loan interest received from borrower
        10  TAX-1098-OUTSTANDING-PRIN PIC S9(13)V99.
        *> Box 2: Outstanding principal
        10  TAX-1098-OVERPAY-REFUND   PIC S9(9)V99.
        *> Box 3: Refund of overpaid interest
    05  TAX-1098-STATUS.
        10  TAX-1098-QUALIFIED        PIC X(1).
        *> Y = Qualified (>= $600.00), N = Below threshold
        10  TAX-1098-FILING-STATUS    PIC X(1).
        *> G = Generated, S = Skipped, E = Error
