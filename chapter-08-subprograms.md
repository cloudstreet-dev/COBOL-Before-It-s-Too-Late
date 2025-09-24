# Chapter 8: Subprograms and Modular COBOL
## How COBOL Does Functions (Spoiler: Painfully)

---

### The Function That Wasn't

In modern languages, functions are first-class citizens:

```javascript
function calculateTax(amount) {
    return amount * 0.08;
}

let tax = calculateTax(100);
```

In COBOL, there are no functions. There are subprograms—separate programs that you CALL:

```cobol
CALL 'CALCTAX' USING WS-AMOUNT WS-TAX.
```

That's right. Every "function" is potentially a separate compiled program. Welcome to modular programming, 1960s style.

### The CALL Statement: COBOL's Function Call

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AMOUNT           PIC 9(7)V99.
       01  WS-TAX-RATE         PIC V999.
       01  WS-TAX-AMOUNT       PIC 9(7)V99.
       01  WS-RETURN-CODE      PIC XX.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 1000.00 TO WS-AMOUNT.
           MOVE .085 TO WS-TAX-RATE.

           CALL 'TAXCALC' USING WS-AMOUNT
                                 WS-TAX-RATE
                                 WS-TAX-AMOUNT
                                 WS-RETURN-CODE.

           IF WS-RETURN-CODE = '00'
               DISPLAY "Tax amount: " WS-TAX-AMOUNT
           ELSE
               DISPLAY "Error in tax calculation"
           END-IF.

           STOP RUN.
```

And here's the subprogram:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAXCALC.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AMOUNT           PIC 9(7)V99.
       01  LS-TAX-RATE         PIC V999.
       01  LS-TAX-AMOUNT       PIC 9(7)V99.
       01  LS-RETURN-CODE      PIC XX.

       PROCEDURE DIVISION USING LS-AMOUNT
                                LS-TAX-RATE
                                LS-TAX-AMOUNT
                                LS-RETURN-CODE.
       CALCULATE-TAX.
           IF LS-AMOUNT IS NUMERIC AND
              LS-TAX-RATE IS NUMERIC
               COMPUTE LS-TAX-AMOUNT = LS-AMOUNT * LS-TAX-RATE
               MOVE '00' TO LS-RETURN-CODE
           ELSE
               MOVE ZERO TO LS-TAX-AMOUNT
               MOVE '99' TO LS-RETURN-CODE
           END-IF.

           EXIT PROGRAM.
```

Note the LINKAGE SECTION—that's where parameters live in a subprogram.

### Static vs Dynamic Calls

```cobol
* Static CALL - subprogram linked at compile time
CALL 'SUBPROG' USING WS-DATA.

* Dynamic CALL - subprogram loaded at runtime
01  WS-PROGRAM-NAME     PIC X(8).

MOVE 'SUBPROG' TO WS-PROGRAM-NAME.
CALL WS-PROGRAM-NAME USING WS-DATA.

* Cancel a dynamic program from memory
CANCEL WS-PROGRAM-NAME.
```

Static calls are faster but create larger executables. Dynamic calls allow program updates without relinking everything.

### Parameter Passing: BY REFERENCE vs BY CONTENT

```cobol
* BY REFERENCE (default) - pass address, changes affect caller
CALL 'SUBPROG' USING WS-DATA.

* BY CONTENT - pass copy, changes don't affect caller
CALL 'SUBPROG' USING BY CONTENT WS-DATA.

* BY VALUE - pass value directly (rare in COBOL)
CALL 'SUBPROG' USING BY VALUE WS-NUMBER.

* Mixed modes
CALL 'SUBPROG' USING BY REFERENCE WS-INPUT-DATA
                     BY CONTENT WS-CONTROL-FLAG
                     BY REFERENCE WS-OUTPUT-DATA.
```

**Warning**: BY REFERENCE is default. Your subprogram can accidentally modify the caller's data!

### COPY Books: COBOL's #include

```cobol
* CUSTDEF.CPY - Customer definition copybook
       01  CUSTOMER-RECORD.
           05 CUST-ID          PIC 9(10).
           05 CUST-NAME        PIC X(30).
           05 CUST-ADDRESS.
              10 CUST-STREET   PIC X(30).
              10 CUST-CITY     PIC X(20).
              10 CUST-STATE    PIC XX.
              10 CUST-ZIP      PIC 9(5).
           05 CUST-BALANCE     PIC S9(7)V99.
```

Using the copybook:

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CUSTDEF.

       * Can also replace text during copy
       COPY CUSTDEF REPLACING ==CUST== BY ==CUSTOMER==.
```

Copybooks ensure consistent data structures across programs. Change the copybook, recompile everything, hope nothing breaks.

### Nested Programs: Functions Inside Programs

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SHARED-DATA      PIC X(100).
       01  WS-RESULT           PIC 9(9).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE "Important data" TO WS-SHARED-DATA.
           CALL 'INNER-PROG' USING WS-RESULT.
           DISPLAY "Result: " WS-RESULT.
           STOP RUN.

      * Nested program starts here
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER-PROG.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-RESULT           PIC 9(9).

       PROCEDURE DIVISION USING LS-RESULT.
           MOVE 42 TO LS-RESULT.
           EXIT PROGRAM.

       END PROGRAM INNER-PROG.
       END PROGRAM MAIN-PROGRAM.
```

Nested programs can access parent's WORKING-STORAGE if declared with COMMON clause. It's like closures, but from 1985.

### FUNCTION: COBOL's Built-in Functions

```cobol
* Intrinsic functions (added in COBOL-89)
COMPUTE WS-RESULT = FUNCTION SQRT(WS-NUMBER).
COMPUTE WS-MAX = FUNCTION MAX(WS-A, WS-B, WS-C).
COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-STRING).
COMPUTE WS-DATE = FUNCTION CURRENT-DATE.

* String functions
MOVE FUNCTION UPPER-CASE(WS-INPUT) TO WS-OUTPUT.
MOVE FUNCTION TRIM(WS-INPUT) TO WS-OUTPUT.
MOVE FUNCTION REVERSE(WS-INPUT) TO WS-OUTPUT.

* Date functions
COMPUTE WS-DAYS = FUNCTION INTEGER-OF-DATE(WS-DATE).
COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(WS-DAYS).

* Financial functions
COMPUTE WS-PAYMENT = FUNCTION ANNUITY(WS-RATE, WS-PERIODS).
```

These are actual functions! They return values! Only took 30 years to add them.

### Real-World Modular Pattern: Validation Service

```cobol
      *================================================================
      * PROGRAM: VALIDATE
      * PURPOSE: Common validation routines
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05 WS-DIGIT-COUNT   PIC 99.
           05 WS-I             PIC 99.

       LINKAGE SECTION.
       01  LS-FUNCTION-CODE    PIC XX.
           88 VALIDATE-SSN     VALUE '01'.
           88 VALIDATE-PHONE   VALUE '02'.
           88 VALIDATE-EMAIL   VALUE '03'.
           88 VALIDATE-ZIP     VALUE '04'.

       01  LS-INPUT-DATA       PIC X(100).
       01  LS-RETURN-CODE      PIC XX.
           88 VALID            VALUE '00'.
           88 INVALID          VALUE '99'.

       PROCEDURE DIVISION USING LS-FUNCTION-CODE
                                LS-INPUT-DATA
                                LS-RETURN-CODE.
       MAIN-LOGIC.
           EVALUATE TRUE
               WHEN VALIDATE-SSN
                   PERFORM VALIDATE-SSN-ROUTINE
               WHEN VALIDATE-PHONE
                   PERFORM VALIDATE-PHONE-ROUTINE
               WHEN VALIDATE-EMAIL
                   PERFORM VALIDATE-EMAIL-ROUTINE
               WHEN VALIDATE-ZIP
                   PERFORM VALIDATE-ZIP-ROUTINE
               WHEN OTHER
                   SET INVALID TO TRUE
           END-EVALUATE.

           EXIT PROGRAM.

       VALIDATE-SSN-ROUTINE.
           * SSN format: 999-99-9999
           IF LS-INPUT-DATA(1:3) IS NUMERIC AND
              LS-INPUT-DATA(4:1) = '-' AND
              LS-INPUT-DATA(5:2) IS NUMERIC AND
              LS-INPUT-DATA(7:1) = '-' AND
              LS-INPUT-DATA(8:4) IS NUMERIC
               SET VALID TO TRUE
           ELSE
               SET INVALID TO TRUE
           END-IF.

       VALIDATE-PHONE-ROUTINE.
           * Phone format: (999) 999-9999
           IF LS-INPUT-DATA(1:1) = '(' AND
              LS-INPUT-DATA(2:3) IS NUMERIC AND
              LS-INPUT-DATA(5:2) = ') ' AND
              LS-INPUT-DATA(7:3) IS NUMERIC AND
              LS-INPUT-DATA(10:1) = '-' AND
              LS-INPUT-DATA(11:4) IS NUMERIC
               SET VALID TO TRUE
           ELSE
               SET INVALID TO TRUE
           END-IF.

       END PROGRAM VALIDATE.
```

### REPOSITORY: Managing Functions

```cobol
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC
           PROGRAM VALIDATE
           PROGRAM CALCULATE-TAX
           PROGRAM FORMAT-OUTPUT.
```

The REPOSITORY paragraph declares what functions and programs are available. It's like import statements, but optional.

### Exception Handling in CALL

```cobol
CALL 'SUBPROG' USING WS-DATA
    ON EXCEPTION
        DISPLAY "Subprogram not found!"
        MOVE 'ERROR' TO WS-STATUS
    NOT ON EXCEPTION
        DISPLAY "Subprogram executed successfully"
END-CALL.

* ON OVERFLOW for arithmetic
CALL 'CALCULATE' USING WS-BIG-NUMBER
    ON OVERFLOW
        DISPLAY "Calculation overflow!"
END-CALL.
```

### Creating a Library of Subprograms

```cobol
      *================================================================
      * STRLIB.COB - String manipulation library
      *================================================================

      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STR-LENGTH.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-STRING           PIC X(1000).
       01  LS-LENGTH           PIC 9(4).

       PROCEDURE DIVISION USING LS-STRING LS-LENGTH.
           MOVE ZERO TO LS-LENGTH.
           PERFORM VARYING LS-LENGTH FROM 1000 BY -1
                   UNTIL LS-LENGTH = 0
                      OR LS-STRING(LS-LENGTH:1) NOT = SPACE
               CONTINUE
           END-PERFORM.
           EXIT PROGRAM.
       END PROGRAM STR-LENGTH.

      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STR-TRIM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LENGTH           PIC 9(4).

       LINKAGE SECTION.
       01  LS-INPUT            PIC X(1000).
       01  LS-OUTPUT           PIC X(1000).

       PROCEDURE DIVISION USING LS-INPUT LS-OUTPUT.
           CALL 'STR-LENGTH' USING LS-INPUT WS-LENGTH.
           MOVE SPACES TO LS-OUTPUT.
           MOVE LS-INPUT(1:WS-LENGTH) TO LS-OUTPUT.
           EXIT PROGRAM.
       END PROGRAM STR-TRIM.
```

### Performance Considerations

1. **Static vs Dynamic**: Static calls are faster but increase program size
2. **Parameter Size**: Large parameters passed BY REFERENCE are more efficient
3. **CANCEL**: Free memory after using dynamic programs
4. **Local vs Working Storage**: LOCAL-STORAGE is reinitialized each call

### Common Subprogram Patterns

#### Pattern 1: Service Program
```cobol
* One program, multiple functions
EVALUATE WS-SERVICE-CODE
    WHEN 'ADD' PERFORM ADD-SERVICE
    WHEN 'UPD' PERFORM UPDATE-SERVICE
    WHEN 'DEL' PERFORM DELETE-SERVICE
    WHEN 'INQ' PERFORM INQUIRY-SERVICE
END-EVALUATE.
```

#### Pattern 2: Error Handler
```cobol
CALL 'ERRORLOG' USING WS-ERROR-CODE
                      WS-ERROR-MESSAGE
                      WS-PROGRAM-NAME
                      WS-TIMESTAMP.
```

#### Pattern 3: Business Rules Engine
```cobol
CALL 'BIZRULES' USING WS-RULE-ID
                      WS-INPUT-DATA
                      WS-OUTPUT-DATA
                      WS-RETURN-CODE.
```

### What You've Learned

COBOL's modular programming is like object-oriented programming if you removed the objects, the orientation, and most of the programming. But it works. Subprograms provide code reuse, copybooks ensure consistency, and the CALL statement has been linking programs together since before linking was cool.

The lack of true functions is painful, but COBOL programmers have built complex, maintainable systems with these tools. It's not elegant, but it's battle-tested.

---

*Next Chapter: [Chapter 9: Database Access with COBOL](chapter-09-database-access.md)*

*"In COBOL, every function call is a long-distance relationship."*