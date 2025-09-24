# Chapter 7: String Manipulation and Report Generation
## The Lost Art of 132-Column Perfection

---

### Strings: COBOL's Biggest Pain Point

In JavaScript, string manipulation is effortless:
```javascript
let name = firstName + " " + lastName;
let email = `${username}@${domain}`;
let parts = fullName.split(" ");
```

In COBOL, string manipulation is like performing surgery with a butter knife:
```cobol
STRING WS-FIRST-NAME DELIMITED BY SPACE
       " " DELIMITED BY SIZE
       WS-LAST-NAME DELIMITED BY SPACE
       INTO WS-FULL-NAME
END-STRING.
```

Welcome to COBOL strings, where concatenation requires a paragraph and splitting strings is a multi-page ordeal.

### The STRING Statement: Concatenation's Verbose Cousin

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST-NAME        PIC X(20) VALUE "John".
       01 WS-LAST-NAME         PIC X(20) VALUE "Smith".
       01 WS-FULL-NAME         PIC X(41).
       01 WS-EMAIL-USER        PIC X(20) VALUE "jsmith".
       01 WS-EMAIL-DOMAIN      PIC X(20) VALUE "example.com".
       01 WS-EMAIL             PIC X(50).
       01 WS-POINTER           PIC 99.

       PROCEDURE DIVISION.
       BUILD-STRINGS.
           * Simple concatenation
           STRING WS-FIRST-NAME DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-LAST-NAME DELIMITED BY SPACE
                  INTO WS-FULL-NAME
           END-STRING.

           * Building email address
           STRING WS-EMAIL-USER DELIMITED BY SPACE
                  "@" DELIMITED BY SIZE
                  WS-EMAIL-DOMAIN DELIMITED BY SPACE
                  INTO WS-EMAIL
           END-STRING.

           * Using POINTER for positioning
           MOVE 1 TO WS-POINTER.
           STRING "Dear " DELIMITED BY SIZE
                  INTO WS-FULL-NAME
                  WITH POINTER WS-POINTER
           END-STRING.
           * WS-POINTER now points to next position
```

DELIMITED BY controls what portion of the source field to use:
- `DELIMITED BY SPACE` - Stop at first space
- `DELIMITED BY SIZE` - Use entire field
- `DELIMITED BY "/"` - Stop at specific character

### The UNSTRING Statement: Splitting's Nightmare

```cobol
       01 WS-FULL-PATH         PIC X(100) VALUE
          "/home/user/documents/report.txt".
       01 WS-PATH-PARTS.
          05 WS-PART OCCURS 10 TIMES PIC X(20).
       01 WS-PART-COUNT        PIC 99.
       01 WS-CSV-LINE          PIC X(200) VALUE
          "Smith,John,12345,Sales,50000".
       01 WS-FIELD-1           PIC X(30).
       01 WS-FIELD-2           PIC X(30).
       01 WS-FIELD-3           PIC X(30).
       01 WS-FIELD-4           PIC X(30).
       01 WS-FIELD-5           PIC X(30).

       * Split path by delimiter
       UNSTRING WS-FULL-PATH
           DELIMITED BY "/"
           INTO WS-PART(1) WS-PART(2) WS-PART(3)
                WS-PART(4) WS-PART(5)
           TALLYING IN WS-PART-COUNT
       END-UNSTRING.

       * Parse CSV line
       UNSTRING WS-CSV-LINE
           DELIMITED BY ","
           INTO WS-FIELD-1
                WS-FIELD-2
                WS-FIELD-3
                WS-FIELD-4
                WS-FIELD-5
       END-UNSTRING.
```

Notice you must specify every destination field. There's no dynamic array population.

### The INSPECT Statement: Search and Replace

```cobol
       01 WS-TEXT              PIC X(100).
       01 WS-CHAR-COUNT        PIC 999.
       01 WS-PHONE             PIC X(14) VALUE "(555) 123-4567".

       * Count occurrences
       MOVE ZERO TO WS-CHAR-COUNT.
       INSPECT WS-TEXT TALLYING WS-CHAR-COUNT
           FOR ALL "A".

       * Replace characters
       INSPECT WS-PHONE REPLACING
           ALL "(" BY SPACE
           ALL ")" BY SPACE
           ALL "-" BY SPACE.
       * Result: " 555  123 4567"

       * Convert to uppercase (if supported)
       INSPECT WS-TEXT CONVERTING
           "abcdefghijklmnopqrstuvwxyz" TO
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       * Complex inspection
       INSPECT WS-TEXT
           TALLYING WS-CHAR-COUNT FOR LEADING SPACES
           REPLACING LEADING SPACES BY ZEROS.
```

### Reference Modification: COBOL's Substring

```cobol
       01 WS-FULL-STRING       PIC X(50) VALUE
          "The quick brown fox jumps over the lazy dog".
       01 WS-SUBSTRING         PIC X(20).

       * Extract substring (position:length)
       MOVE WS-FULL-STRING(5:5) TO WS-SUBSTRING.
       * Result: "quick"

       * Dynamic positioning
       01 WS-START             PIC 99 VALUE 11.
       01 WS-LENGTH            PIC 99 VALUE 5.

       MOVE WS-FULL-STRING(WS-START:WS-LENGTH) TO WS-SUBSTRING.
       * Result: "brown"

       * Modify part of string
       MOVE "BLACK" TO WS-FULL-STRING(11:5).
       * String is now "The quick BLACK fox..."
```

### Report Generation: COBOL's Hidden Superpower

COBOL was built for reports. The REPORT SECTION (rarely used now) could generate complex reports declaratively:

```cobol
       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "SALES.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-LINE         PIC X(132).

       WORKING-STORAGE SECTION.
       * Report headers
       01  WS-REPORT-HEADER.
           05 FILLER           PIC X(50) VALUE SPACES.
           05 FILLER           PIC X(32) VALUE
              "MONTHLY SALES REPORT".
           05 FILLER           PIC X(50) VALUE SPACES.

       01  WS-COLUMN-HEADERS.
           05 FILLER           PIC X(10) VALUE "REGION".
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(20) VALUE "SALESPERSON".
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(12) VALUE "PRODUCT".
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(10) VALUE "QUANTITY".
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(12) VALUE "AMOUNT".

       * Detail line
       01  WS-DETAIL-LINE.
           05 DL-REGION        PIC X(10).
           05 FILLER           PIC X(5) VALUE SPACES.
           05 DL-SALESPERSON   PIC X(20).
           05 FILLER           PIC X(5) VALUE SPACES.
           05 DL-PRODUCT       PIC X(12).
           05 FILLER           PIC X(5) VALUE SPACES.
           05 DL-QUANTITY      PIC ZZZ,ZZ9.
           05 FILLER           PIC X(5) VALUE SPACES.
           05 DL-AMOUNT        PIC $$$$,$$9.99.

       * Total lines
       01  WS-TOTAL-LINE.
           05 FILLER           PIC X(47) VALUE SPACES.
           05 FILLER           PIC X(20) VALUE
              "TOTAL:".
           05 FILLER           PIC X(5) VALUE SPACES.
           05 TL-TOTAL-AMOUNT  PIC $$$,$$$,$$9.99.

       01  WS-PAGE-NUMBER      PIC 999 VALUE 1.
       01  WS-LINE-COUNT       PIC 99 VALUE 99.
       01  WS-MAX-LINES        PIC 99 VALUE 60.

       PROCEDURE DIVISION.
       PRODUCE-REPORT.
           OPEN OUTPUT REPORT-FILE.
           PERFORM PRINT-HEADERS.
           PERFORM PROCESS-ALL-SALES.
           PERFORM PRINT-TOTALS.
           CLOSE REPORT-FILE.

       PRINT-HEADERS.
           IF WS-LINE-COUNT > WS-MAX-LINES - 5
               PERFORM PAGE-BREAK
           END-IF.

           MOVE WS-REPORT-HEADER TO REPORT-LINE.
           WRITE REPORT-LINE.

           MOVE SPACES TO REPORT-LINE.
           WRITE REPORT-LINE.

           MOVE WS-COLUMN-HEADERS TO REPORT-LINE.
           WRITE REPORT-LINE.

           MOVE ALL "-" TO REPORT-LINE.
           WRITE REPORT-LINE.

           MOVE 4 TO WS-LINE-COUNT.

       PRINT-DETAIL.
           IF WS-LINE-COUNT > WS-MAX-LINES
               PERFORM PAGE-BREAK
               PERFORM PRINT-HEADERS
           END-IF.

           MOVE SALE-REGION TO DL-REGION.
           MOVE SALE-PERSON TO DL-SALESPERSON.
           MOVE SALE-PRODUCT TO DL-PRODUCT.
           MOVE SALE-QUANTITY TO DL-QUANTITY.
           MOVE SALE-AMOUNT TO DL-AMOUNT.

           MOVE WS-DETAIL-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.

           ADD 1 TO WS-LINE-COUNT.

       PAGE-BREAK.
           MOVE SPACES TO REPORT-LINE.
           STRING "Page " WS-PAGE-NUMBER
               INTO REPORT-LINE.
           WRITE REPORT-LINE AFTER PAGE.
           ADD 1 TO WS-PAGE-NUMBER.
           MOVE 1 TO WS-LINE-COUNT.
```

### Control Break Processing: Reports with Subtotals

```cobol
       01  WS-PREVIOUS-REGION  PIC X(10) VALUE SPACES.
       01  WS-REGION-TOTAL     PIC 9(9)V99 VALUE ZERO.
       01  WS-GRAND-TOTAL      PIC 9(9)V99 VALUE ZERO.

       PROCESS-WITH-CONTROL-BREAKS.
           PERFORM UNTIL END-OF-FILE
               READ SALES-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                       IF WS-PREVIOUS-REGION NOT = SPACES
                           PERFORM REGION-BREAK
                       END-IF
                   NOT AT END
                       IF SALE-REGION NOT = WS-PREVIOUS-REGION
                           IF WS-PREVIOUS-REGION NOT = SPACES
                               PERFORM REGION-BREAK
                           END-IF
                           MOVE SALE-REGION TO WS-PREVIOUS-REGION
                       END-IF
                       PERFORM PROCESS-SALE-RECORD
               END-READ
           END-PERFORM.

           PERFORM PRINT-GRAND-TOTAL.

       REGION-BREAK.
           PERFORM PRINT-REGION-TOTAL.
           ADD WS-REGION-TOTAL TO WS-GRAND-TOTAL.
           MOVE ZERO TO WS-REGION-TOTAL.

       PROCESS-SALE-RECORD.
           ADD SALE-AMOUNT TO WS-REGION-TOTAL.
           PERFORM PRINT-DETAIL.

       PRINT-REGION-TOTAL.
           MOVE SPACES TO REPORT-LINE.
           STRING "Region " WS-PREVIOUS-REGION
                  " Total: $" WS-REGION-TOTAL
               INTO REPORT-LINE.
           WRITE REPORT-LINE.
```

### Formatting Functions: Modern COBOL String Helpers

```cobol
* TRIM function (COBOL 2002+)
01 WS-PADDED            PIC X(30) VALUE "  HELLO WORLD  ".
01 WS-TRIMMED           PIC X(30).

MOVE FUNCTION TRIM(WS-PADDED) TO WS-TRIMMED.
* Result: "HELLO WORLD"

* UPPER-CASE and LOWER-CASE
MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-UPPER.
MOVE FUNCTION LOWER-CASE(WS-TEXT) TO WS-LOWER.

* LENGTH function
COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-TEXT).

* REVERSE function
MOVE FUNCTION REVERSE(WS-TEXT) TO WS-BACKWARDS.
```

### JSON and XML: COBOL in the Modern World

Modern COBOL can handle JSON and XML:

```cobol
* JSON GENERATE (COBOL 2002+)
01  WS-CUSTOMER-RECORD.
    05 CUSTOMER-ID      PIC 9(10).
    05 CUSTOMER-NAME    PIC X(30).
    05 CUSTOMER-BALANCE PIC 9(7)V99.

01  WS-JSON-STRING      PIC X(1000).

JSON GENERATE WS-JSON-STRING FROM WS-CUSTOMER-RECORD.

* XML GENERATE
01  WS-XML-STRING       PIC X(1000).

XML GENERATE WS-XML-STRING FROM WS-CUSTOMER-RECORD.
```

But most legacy systems predate these features.

### Real-World Example: Format a Check

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-WRITER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CHECK-AMOUNT     PIC 9(7)V99.
       01  WS-AMOUNT-IN-WORDS  PIC X(100).
       01  WS-DOLLARS          PIC 9(7).
       01  WS-CENTS            PIC 99.

       01  WS-UNITS.
           05 FILLER PIC X(9) VALUE "ONE      ".
           05 FILLER PIC X(9) VALUE "TWO      ".
           05 FILLER PIC X(9) VALUE "THREE    ".
           05 FILLER PIC X(9) VALUE "FOUR     ".
           05 FILLER PIC X(9) VALUE "FIVE     ".
           05 FILLER PIC X(9) VALUE "SIX      ".
           05 FILLER PIC X(9) VALUE "SEVEN    ".
           05 FILLER PIC X(9) VALUE "EIGHT    ".
           05 FILLER PIC X(9) VALUE "NINE     ".

       01  WS-UNITS-TABLE REDEFINES WS-UNITS.
           05 WS-UNIT OCCURS 9 TIMES PIC X(9).

       01  WS-TENS.
           05 FILLER PIC X(9) VALUE "TWENTY   ".
           05 FILLER PIC X(9) VALUE "THIRTY   ".
           05 FILLER PIC X(9) VALUE "FORTY    ".
           05 FILLER PIC X(9) VALUE "FIFTY    ".
           05 FILLER PIC X(9) VALUE "SIXTY    ".
           05 FILLER PIC X(9) VALUE "SEVENTY  ".
           05 FILLER PIC X(9) VALUE "EIGHTY   ".
           05 FILLER PIC X(9) VALUE "NINETY   ".

       01  WS-TENS-TABLE REDEFINES WS-TENS.
           05 WS-TEN OCCURS 8 TIMES PIC X(9).

       PROCEDURE DIVISION.
       FORMAT-CHECK.
           MOVE 1234.56 TO WS-CHECK-AMOUNT.
           DIVIDE WS-CHECK-AMOUNT BY 1
               GIVING WS-DOLLARS REMAINDER WS-CENTS.

           PERFORM CONVERT-TO-WORDS.
           PERFORM PRINT-CHECK.

       PRINT-CHECK.
           DISPLAY "****************************************".
           DISPLAY "* PAY TO THE ORDER OF: ________________".
           DISPLAY "* ".
           STRING "* $" WS-CHECK-AMOUNT
               INTO WS-TEMP.
           DISPLAY WS-TEMP.
           DISPLAY "* ".
           STRING "* " WS-AMOUNT-IN-WORDS
               INTO WS-TEMP.
           DISPLAY WS-TEMP.
           DISPLAY "****************************************".
```

### Common String Patterns

#### Pattern 1: Trim Trailing Spaces
```cobol
01  WS-TEXT             PIC X(50).
01  WS-LENGTH           PIC 99.

MOVE 50 TO WS-LENGTH.
PERFORM UNTIL WS-LENGTH = 0
    OR WS-TEXT(WS-LENGTH:1) NOT = SPACE
    SUBTRACT 1 FROM WS-LENGTH
END-PERFORM.
* WS-LENGTH now contains actual text length
```

#### Pattern 2: Center Text
```cobol
01  WS-TEXT             PIC X(20) VALUE "HELLO".
01  WS-CENTERED         PIC X(80).
01  WS-TEXT-LEN         PIC 99.
01  WS-START-POS        PIC 99.

* Calculate centering position
COMPUTE WS-TEXT-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-TEXT)).
COMPUTE WS-START-POS = (80 - WS-TEXT-LEN) / 2 + 1.
MOVE SPACES TO WS-CENTERED.
MOVE WS-TEXT TO WS-CENTERED(WS-START-POS:WS-TEXT-LEN).
```

### String Gotchas

```cobol
* Gotcha 1: Fixed-width padding
01 WS-NAME PIC X(20) VALUE "BOB".
* WS-NAME is "BOB                 " (17 spaces!)

* Gotcha 2: STRING doesn't clear destination
MOVE "PREVIOUS CONTENT HERE" TO WS-OUTPUT.
STRING "NEW" INTO WS-OUTPUT.
* Result: "NEWVIOUS CONTENT HERE"

* Gotcha 3: UNSTRING needs enough destinations
UNSTRING "A,B,C,D,E" DELIMITED BY ","
    INTO WS-F1 WS-F2 WS-F3.
* Only gets first 3, rest are lost

* Gotcha 4: Reference modification bounds
MOVE WS-TEXT(51:10) TO WS-OUTPUT.  * Runtime error if WS-TEXT < 60 chars
```

### Performance Tips

1. **Use reference modification over STRING/UNSTRING for simple cases**
2. **Pre-calculate report layouts at compile time**
3. **Use INSPECT for counting/replacing over manual loops**
4. **Initialize report buffers once, reuse them**

### What You've Learned

COBOL string manipulation is verbose, inflexible, and frustrating. But it's also explicit, predictable, and has been formatting checks and reports for 60 years. The report generation capabilities, while archaic, are actually quite powerful—modern reporting tools essentially do the same thing with prettier syntax.

---

*Next Chapter: [Chapter 8: Subprograms and Modular COBOL](chapter-08-subprograms.md)*

*"In COBOL, concatenation is not '+', it's a paragraph with a thesis statement."*