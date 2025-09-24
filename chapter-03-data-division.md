# Chapter 3: Data Division Deep Dive
## Where Variables Go to Get Their Life Together

---

### The Data Division Philosophy

In JavaScript, you can declare a variable anywhere, anytime, as anything:

```javascript
let x = 5;
x = "now I'm a string!";
x = { now: "I'm an object!" };
x = undefined; // I give up
```

COBOL looks at this chaos and says, "Absolutely not." In COBOL, every piece of data is declared upfront, with its exact size, type, and initial value. It's like a prison, but for variables. And the warden is the DATA DIVISION.

### The Four Sections of Data Division

The DATA DIVISION has four sections, though you'll usually only use two:

1. **FILE SECTION** - Describes files and their records
2. **WORKING-STORAGE SECTION** - Variables that persist throughout the program
3. **LOCAL-STORAGE SECTION** - Variables that reset on each call (for subprograms)
4. **LINKAGE SECTION** - Parameters passed between programs

Think of it like organizing your closet:
- FILE: The clothes in the laundry basket (external data)
- WORKING-STORAGE: Your everyday wardrobe (always available)
- LOCAL-STORAGE: That outfit you only wear once (temporary)
- LINKAGE: Clothes you borrow from friends (shared data)

### Working-Storage: Your Variable Home

Let's start with WORKING-STORAGE, where 90% of your variables will live:

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Simple variables
       01 WS-COUNTER         PIC 9(3) VALUE ZERO.
       01 WS-NAME            PIC X(30) VALUE SPACES.
       01 WS-BALANCE         PIC S9(7)V99 VALUE ZERO.
       01 WS-FLAG            PIC X VALUE 'N'.

       * A complex structure
       01 WS-EMPLOYEE-RECORD.
          05 WS-EMPLOYEE-ID       PIC 9(6).
          05 WS-EMPLOYEE-NAME.
             10 WS-FIRST-NAME     PIC X(15).
             10 WS-MIDDLE-INITIAL PIC X.
             10 WS-LAST-NAME      PIC X(20).
          05 WS-EMPLOYEE-ADDRESS.
             10 WS-STREET         PIC X(30).
             10 WS-CITY           PIC X(20).
             10 WS-STATE          PIC XX.
             10 WS-ZIP            PIC 9(5).
          05 WS-SALARY            PIC 9(7)V99.
          05 WS-HIRE-DATE.
             10 WS-HIRE-YEAR      PIC 9(4).
             10 WS-HIRE-MONTH     PIC 99.
             10 WS-HIRE-DAY       PIC 99.
```

### Level Numbers: The Hierarchy System

Level numbers create structure. Here's what they mean:

- **01**: Top-level item (like a root object)
- **02-49**: Subordinate items (nested properties)
- **66**: RENAMES clause (aliasing, rarely used)
- **77**: Independent elementary item (standalone variable)
- **88**: Condition name (boolean flags)

The numbers don't have to be sequential. This is perfectly valid:

```cobol
01 WS-CUSTOMER.
   05 WS-CUST-ID     PIC 9(6).
   05 WS-CUST-NAME   PIC X(30).
   10 WS-CUST-PHONE  PIC X(10).  * Wait, 10 under 05? Yes, it's fine!
```

Why skip numbers? Legacy programmers left gaps for future insertions. Like leaving space between functions "in case we need to add something."

### The PICTURE Clause Expanded

The PIC clause is COBOL's type system, regex pattern, and validator all in one:

```cobol
* Numeric pictures
01 WS-INTEGER         PIC 9(5).        * 00000 to 99999
01 WS-SIGNED          PIC S9(5).       * -99999 to +99999
01 WS-DECIMAL         PIC 9(3)V99.     * 000.00 to 999.99
01 WS-COMP-3          PIC S9(7)V99 COMP-3.  * Packed decimal (saves space)

* Alphanumeric pictures
01 WS-TEXT            PIC X(50).       * Any 50 characters
01 WS-ALPHA           PIC A(10).       * Letters only
01 WS-ALPHANUM        PIC X(10).       * Letters and numbers

* Edited pictures (for display)
01 WS-DISPLAY-MONEY   PIC $$$,$$9.99.  * $  1,234.56
01 WS-DISPLAY-DATE    PIC 99/99/9999.  * 12/31/2025
01 WS-DISPLAY-SSN     PIC 999-99-9999. * 123-45-6789
01 WS-SUPPRESSED      PIC ZZZ,ZZ9.99.  * Leading zeros become spaces
```

Here's where it gets fun. COBOL can format your data automatically:

```cobol
01 WS-RAW-AMOUNT      PIC 9(7)V99 VALUE 1234.56.
01 WS-DISPLAY-AMOUNT  PIC $$$,$$9.99.

MOVE WS-RAW-AMOUNT TO WS-DISPLAY-AMOUNT.
DISPLAY WS-DISPLAY-AMOUNT.  * Shows: $  1,234.56
```

JavaScript developers: How many times have you written currency formatters? COBOL had this in 1959.

### VALUE Clause: Initial Values Done Right

Every variable can have an initial value:

```cobol
01 WS-COUNTER         PIC 9(3) VALUE 0.
01 WS-MESSAGE         PIC X(20) VALUE "Hello, COBOL!".
01 WS-FLAG            PIC X VALUE 'Y'.
01 WS-PRICE           PIC 9(5)V99 VALUE 199.99.

* Special values
01 WS-BLANK-NAME      PIC X(30) VALUE SPACES.
01 WS-ZERO-BALANCE    PIC 9(7)V99 VALUE ZEROS.
01 WS-HIGH-VALUE      PIC X(10) VALUE HIGH-VALUES.
01 WS-LOW-VALUE       PIC X(10) VALUE LOW-VALUES.
```

What are HIGH-VALUES and LOW-VALUES? They're the maximum and minimum values for a field's collating sequence. Think `\xFF` and `\x00` if you're into that sort of thing. Useful for sorting and boundary conditions.

### REDEFINES: COBOL's Union Types

REDEFINES lets you look at the same memory differently:

```cobol
01 WS-DATE-NUMERIC    PIC 9(8).
01 WS-DATE-FORMATTED  REDEFINES WS-DATE-NUMERIC.
   05 WS-YEAR         PIC 9(4).
   05 WS-MONTH        PIC 99.
   05 WS-DAY          PIC 99.

* Now you can:
MOVE 20250115 TO WS-DATE-NUMERIC.
DISPLAY "Year: " WS-YEAR.    * Shows: 2025
DISPLAY "Month: " WS-MONTH.   * Shows: 01
DISPLAY "Day: " WS-DAY.       * Shows: 15
```

Or more practically:

```cobol
01 WS-EMPLOYEE-ID     PIC X(9).
01 WS-EMPLOYEE-PARTS  REDEFINES WS-EMPLOYEE-ID.
   05 WS-DEPT-CODE    PIC XXX.
   05 WS-LOCATION     PIC XX.
   05 WS-EMP-NUMBER   PIC 9(4).

MOVE "IT NY1234" TO WS-EMPLOYEE-ID.
* Now WS-DEPT-CODE = "IT "
* WS-LOCATION = "NY"
* WS-EMP-NUMBER = 1234
```

This is like union types in C, but predating C by a decade.

### 88 Levels: The Elegant Boolean

COBOL doesn't have boolean types. Instead, it has condition names:

```cobol
01 WS-MARITAL-STATUS  PIC X.
   88 SINGLE          VALUE 'S'.
   88 MARRIED         VALUE 'M'.
   88 DIVORCED        VALUE 'D'.
   88 WIDOWED         VALUE 'W'.
   88 VALID-STATUS    VALUES 'S' 'M' 'D' 'W'.

01 WS-ACCOUNT-BALANCE PIC S9(7)V99.
   88 OVERDRAWN       VALUE -999999.99 THRU -0.01.
   88 ZERO-BALANCE    VALUE ZERO.
   88 HAS-FUNDS       VALUE 0.01 THRU 999999.99.

* Usage:
IF MARRIED
    PERFORM APPLY-JOINT-FILING
END-IF.

IF OVERDRAWN
    PERFORM CHARGE-OVERDRAFT-FEE
END-IF.
```

This is actually brilliant. Instead of magic strings scattered throughout your code, you define conditions once and name them meaningfully.

### OCCURS: Arrays, COBOL Style

COBOL's arrays are... different:

```cobol
01 WS-MONTHLY-SALES.
   05 WS-MONTH-SALES  PIC 9(7)V99 OCCURS 12 TIMES.

* Access with subscripts (1-indexed, not 0!)
MOVE 10000.00 TO WS-MONTH-SALES(1).  * January
MOVE 12000.00 TO WS-MONTH-SALES(2).  * February

* Or with indexes (more efficient)
01 WS-MONTH-INDEX     PIC 99.

PERFORM VARYING WS-MONTH-INDEX FROM 1 BY 1
        UNTIL WS-MONTH-INDEX > 12
    DISPLAY "Month " WS-MONTH-INDEX ": "
            WS-MONTH-SALES(WS-MONTH-INDEX)
END-PERFORM.
```

Multi-dimensional arrays? Sure:

```cobol
01 WS-SALES-MATRIX.
   05 WS-REGION OCCURS 4 TIMES.
      10 WS-QUARTERLY-SALES PIC 9(7)V99 OCCURS 4 TIMES.

* Access: WS-QUARTERLY-SALES(2, 3) = Region 2, Quarter 3
```

### COMP Fields: Storage Optimization

COBOL has several computational formats:

```cobol
* Normal display format (one byte per digit)
01 WS-DISPLAY     PIC 9(5) VALUE 12345.         * 5 bytes: "12345"

* Binary format (faster math)
01 WS-BINARY      PIC 9(5) COMP VALUE 12345.    * 2-4 bytes

* Packed decimal (two digits per byte)
01 WS-PACKED      PIC 9(5) COMP-3 VALUE 12345.  * 3 bytes

* Floating point (finally added in COBOL 2002)
01 WS-FLOAT       COMP-1.                       * 4 bytes
01 WS-DOUBLE      COMP-2.                       * 8 bytes
```

Why care? Because that COBOL program processing millions of records saves gigabytes with COMP-3. In 1970, that mattered. In 2025 on a mainframe charging by CPU cycle and storage, it still matters.

### File Section: Where External Data Lives

When working with files, you declare their structure:

```cobol
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUST-ID        PIC 9(6).
           05 CUST-NAME      PIC X(30).
           05 CUST-BALANCE   PIC S9(7)V99.
           05 CUST-STATUS    PIC X.
              88 ACTIVE      VALUE 'A'.
              88 INACTIVE    VALUE 'I'.
              88 SUSPENDED   VALUE 'S'.
```

This tells COBOL exactly how to interpret each record in the file. No JSON parsing, no schema validation—the structure IS the schema.

### The COPY Statement: COBOL's Include

Large organizations have standard data structures. Instead of redefining them everywhere:

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Include standard customer definition
       COPY CUSTDEF.

       * Include standard error codes
       COPY ERRORCDS.
```

The COPY statement literally copies the text from another file. It's `#include` from C, but older.

### Practical Example: A Complete Data Structure

Let's build something real: an order processing structure.

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Configuration constants
       01 WS-CONFIG.
          05 WS-MAX-LINE-ITEMS    PIC 99 VALUE 50.
          05 WS-TAX-RATE          PIC V999 VALUE .085.
          05 WS-DISCOUNT-LEVELS.
             10 WS-GOLD-DISCOUNT  PIC V99 VALUE .20.
             10 WS-SILVER-DISCOUNT PIC V99 VALUE .10.
             10 WS-BRONZE-DISCOUNT PIC V99 VALUE .05.

       * Order header
       01 WS-ORDER-HEADER.
          05 WS-ORDER-NUMBER       PIC 9(10).
          05 WS-ORDER-DATE.
             10 WS-ORDER-YEAR      PIC 9(4).
             10 WS-ORDER-MONTH     PIC 99.
             10 WS-ORDER-DAY       PIC 99.
          05 WS-CUSTOMER-INFO.
             10 WS-CUSTOMER-ID     PIC 9(6).
             10 WS-CUSTOMER-NAME   PIC X(30).
             10 WS-CUSTOMER-TYPE   PIC X.
                88 GOLD-CUSTOMER   VALUE 'G'.
                88 SILVER-CUSTOMER VALUE 'S'.
                88 BRONZE-CUSTOMER VALUE 'B'.
                88 REGULAR-CUSTOMER VALUE 'R'.
          05 WS-SHIPPING-ADDRESS.
             10 WS-SHIP-STREET     PIC X(30).
             10 WS-SHIP-CITY       PIC X(20).
             10 WS-SHIP-STATE      PIC XX.
             10 WS-SHIP-ZIP        PIC 9(5).

       * Order details
       01 WS-ORDER-DETAILS.
          05 WS-LINE-COUNT         PIC 99 VALUE ZERO.
          05 WS-ORDER-LINE OCCURS 50 TIMES
                           INDEXED BY LINE-IDX.
             10 WS-PRODUCT-CODE    PIC X(10).
             10 WS-PRODUCT-DESC    PIC X(30).
             10 WS-QUANTITY        PIC 9(3).
             10 WS-UNIT-PRICE      PIC 9(5)V99.
             10 WS-LINE-TOTAL      PIC 9(7)V99.

       * Order totals
       01 WS-ORDER-TOTALS.
          05 WS-SUBTOTAL           PIC 9(9)V99 VALUE ZERO.
          05 WS-DISCOUNT-AMOUNT    PIC 9(9)V99 VALUE ZERO.
          05 WS-TAXABLE-AMOUNT     PIC 9(9)V99 VALUE ZERO.
          05 WS-TAX-AMOUNT         PIC 9(9)V99 VALUE ZERO.
          05 WS-GRAND-TOTAL        PIC 9(9)V99 VALUE ZERO.

       * Display fields (formatted)
       01 WS-DISPLAY-FIELDS.
          05 WS-DISPLAY-PRICE      PIC $$$,$$9.99.
          05 WS-DISPLAY-TOTAL      PIC $$$$,$$9.99.
          05 WS-DISPLAY-DATE       PIC 99/99/9999.
```

This structure can handle a complete order with up to 50 line items, customer information, shipping details, and automatic calculation fields. In JavaScript, you'd need a class, validation logic, and probably a library or two.

### Data Validation: Built Into the Language

COBOL's data definitions provide automatic validation:

```cobol
01 WS-SSN             PIC 999-99-9999.

* This will fail:
MOVE "ABC-DE-FGHI" TO WS-SSN.  * Not numeric!

01 WS-PHONE           PIC X(10).
   88 VALID-PHONE     VALUE "0000000000" THRU "9999999999".

IF NOT VALID-PHONE
    DISPLAY "Invalid phone number"
END-IF.
```

### Memory Alignment and Performance

COBOL programmers obsess about field alignment:

```cobol
* Poor alignment (odd boundaries)
01 WS-POOR-RECORD.
   05 WS-FIELD1       PIC X(3).
   05 WS-FIELD2       PIC 9(5) COMP.
   05 WS-FIELD3       PIC X(2).

* Better alignment (word boundaries)
01 WS-GOOD-RECORD.
   05 WS-FIELD1       PIC X(4).      * Aligned to 4
   05 WS-FIELD2       PIC 9(9) COMP. * Aligned to word
   05 WS-FIELD3       PIC X(4).      * Aligned to 4
```

On mainframes, misaligned data can double processing time. Your JavaScript doesn't care. The mainframe definitely does.

### Common Data Division Patterns

#### Pattern 1: Status Flags
```cobol
01 WS-PROCESSING-FLAGS.
   05 WS-EOF-FLAG          PIC X VALUE 'N'.
      88 END-OF-FILE       VALUE 'Y'.
      88 NOT-END-OF-FILE   VALUE 'N'.
   05 WS-ERROR-FLAG        PIC X VALUE 'N'.
      88 ERROR-OCCURRED    VALUE 'Y'.
      88 NO-ERROR          VALUE 'N'.
```

#### Pattern 2: Counters and Totals
```cobol
01 WS-COUNTERS.
   05 WS-RECORD-COUNT      PIC 9(9) VALUE ZERO.
   05 WS-ERROR-COUNT       PIC 9(9) VALUE ZERO.
   05 WS-SUCCESS-COUNT     PIC 9(9) VALUE ZERO.
```

#### Pattern 3: Work Areas
```cobol
01 WS-WORK-AREAS.
   05 WS-TEMP-STRING       PIC X(100).
   05 WS-CALC-FIELD        PIC 9(15)V99.
   05 WS-FORMATTED-OUTPUT  PIC X(132).  * Standard print line
```

### The Dark Side: What Can Go Wrong

1. **Numeric Overflow**: Move 1000 to PIC 999, get 000
2. **Truncation**: Move "HELLO WORLD" to PIC X(5), get "HELLO"
3. **Invalid Data**: Numeric fields with spaces = computation errors
4. **REDEFINES Chaos**: Change one field, break another
5. **Off-by-one OCCURS**: Arrays are 1-indexed, not 0!

### Modern COBOL Data Features

Recent COBOL standards added:

```cobol
* Dynamic memory allocation (finally!)
01 WS-DYNAMIC-TABLE.
   05 WS-TABLE-SIZE        PIC 9(4).
   05 WS-TABLE-ENTRY       PIC X(100)
                           OCCURS 1 TO 9999 TIMES
                           DEPENDING ON WS-TABLE-SIZE.

* Bit fields
01 WS-STATUS-BITS         PIC 1(8) BIT.

* Unicode support
01 WS-UNICODE-TEXT        PIC N(50).  * National characters
```

But most production COBOL predates these luxuries.

### What You've Learned

The DATA DIVISION is COBOL's answer to type safety, taken to an extreme JavaScript developers can barely imagine. Every byte is accounted for, every field has a specific format, and nothing is left to chance.

This isn't over-engineering—it's engineering for systems that can't fail. When your program processes payroll for a million employees, you don't want dynamic typing surprises.

### Next Chapter Preview

In Chapter 4, we'll explore the PROCEDURE DIVISION, where your carefully declared data finally does something. You'll learn about:
- PERFORM loops (COBOL's control structures)
- The infamous GO TO (and why it's not always evil)
- EVALUATE statements (COBOL's switch on steroids)
- Paragraph and section organization
- How to write maintainable COBOL (yes, it's possible)

Take a moment to appreciate that COBOL figured out data validation, type safety, and memory optimization while other languages were still being invented. Then prepare yourself for the PROCEDURE DIVISION, where COBOL really shows its age—in both good and bad ways.

---

*Next Chapter: [Chapter 4: Procedure Division and Control Flow](chapter-04-procedure-division.md)*

*"In COBOL, we don't have type errors. We have type certainties."*