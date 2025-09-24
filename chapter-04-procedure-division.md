# Chapter 4: Procedure Division and Control Flow
## Where the Magic Happens (Very, Very Verbosely)

---

### Finally, We Can DO Something

After three divisions of setup, we reach the PROCEDURE DIVISION—where COBOL actually computes. If the DATA DIVISION is where variables get dressed, the PROCEDURE DIVISION is where they go to work.

```cobol
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Finally! Actual code!"
           STOP RUN.
```

That's it. That's the simplest PROCEDURE DIVISION. But of course, COBOL is never that simple.

### The Structure: Sections and Paragraphs

COBOL organizes code into sections and paragraphs, like a business document (sensing a pattern?):

```cobol
       PROCEDURE DIVISION.

       MAIN-SECTION SECTION.
       INITIALIZATION.
           DISPLAY "Starting processing...".
           PERFORM LOAD-CONFIG.
           PERFORM OPEN-FILES.

       LOAD-CONFIG.
           MOVE 0.08 TO WS-TAX-RATE.
           MOVE 100 TO WS-MIN-ORDER.

       OPEN-FILES.
           OPEN INPUT CUSTOMER-FILE.
           OPEN OUTPUT REPORT-FILE.

       PROCESSING-SECTION SECTION.
       PROCESS-RECORDS.
           PERFORM READ-AND-PROCESS
               UNTIL END-OF-FILE.

       READ-AND-PROCESS.
           READ CUSTOMER-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END PERFORM PROCESS-ONE-RECORD
           END-READ.
```

Paragraphs are like functions, but they're not. They're more like labeled blocks of code. You can PERFORM them (call them), but they don't have parameters or return values. They're just... there.

### PERFORM: COBOL's Swiss Army Knife

PERFORM is how COBOL does subroutines, loops, and control flow:

```cobol
* Simple PERFORM (like a function call)
PERFORM CALCULATE-TAX.

* PERFORM TIMES (for loop with counter)
PERFORM DISPLAY-STARS 10 TIMES.

* PERFORM UNTIL (while loop)
PERFORM READ-RECORD
    UNTIL END-OF-FILE.

* PERFORM VARYING (for loop with variable)
PERFORM VARYING WS-INDEX FROM 1 BY 1
        UNTIL WS-INDEX > 10
    DISPLAY "Index: " WS-INDEX
END-PERFORM.

* PERFORM THRU (execute multiple paragraphs)
PERFORM INIT-PARA THRU INIT-PARA-EXIT.
```

Here's where it gets interesting. PERFORM can be inline or out-of-line:

```cobol
* Out-of-line PERFORM (jumps to paragraph)
PERFORM CALCULATE-TOTAL.

* Inline PERFORM (code right here)
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
    ADD WS-AMOUNT(WS-I) TO WS-TOTAL
END-PERFORM.
```

### The GO TO Controversy

Yes, COBOL has GO TO. No, you shouldn't use it. Unless you should. It's complicated.

```cobol
       CHECK-BALANCE.
           IF WS-BALANCE < 0
               GO TO ERROR-ROUTINE
           END-IF.
           DISPLAY "Balance OK".
           GO TO CHECK-EXIT.

       ERROR-ROUTINE.
           DISPLAY "ERROR: Negative balance!".
           MOVE 'E' TO WS-STATUS.

       CHECK-EXIT.
           EXIT.
```

**Historical Note**: In the 1960s, GO TO was normal. Dijkstra's "GO TO Considered Harmful" was published in 1968, but COBOL was already 9 years old with millions of lines of GO TO-based code.

Modern COBOL style avoids GO TO except for:
1. Error handling (jumping to error routines)
2. Complex condition exits
3. Maintaining 40-year-old spaghetti code

### IF Statements: Verbose But Clear

COBOL's IF statements spell everything out:

```cobol
IF WS-BALANCE > 1000
    DISPLAY "Premium customer"
ELSE
    DISPLAY "Regular customer"
END-IF.

* Nested IF (the nightmare begins)
IF WS-CUSTOMER-TYPE = 'G'
    IF WS-BALANCE > 10000
        IF WS-YEARS-ACTIVE > 5
            MOVE 0.25 TO WS-DISCOUNT-RATE
        ELSE
            MOVE 0.20 TO WS-DISCOUNT-RATE
        END-IF
    ELSE
        MOVE 0.15 TO WS-DISCOUNT-RATE
    END-IF
ELSE
    IF WS-CUSTOMER-TYPE = 'S'
        MOVE 0.10 TO WS-DISCOUNT-RATE
    ELSE
        MOVE 0.05 TO WS-DISCOUNT-RATE
    END-IF
END-IF.
```

Notice the END-IF? That's COBOL-85. Before that, periods terminated statements, leading to the infamous "period problem":

```cobol
* Old style (COBOL-74)
IF WS-BALANCE > 1000
    DISPLAY "Premium"
    MOVE 'P' TO WS-STATUS.  * Oops! Period ends the IF!
    ADD 1 TO WS-PREMIUM-COUNT.  * This always executes!
```

One misplaced period could change your program's logic. Veterans still have nightmares.

### EVALUATE: The Super Switch

EVALUATE is COBOL's switch statement on steroids:

```cobol
* Simple EVALUATE (like switch)
EVALUATE WS-CUSTOMER-TYPE
    WHEN 'G'
        PERFORM GOLD-PROCESSING
    WHEN 'S'
        PERFORM SILVER-PROCESSING
    WHEN 'B'
        PERFORM BRONZE-PROCESSING
    WHEN OTHER
        PERFORM STANDARD-PROCESSING
END-EVALUATE.

* Multiple conditions
EVALUATE TRUE
    WHEN WS-BALANCE > 10000
        MOVE 'PREMIUM' TO WS-CATEGORY
    WHEN WS-BALANCE > 1000
        MOVE 'STANDARD' TO WS-CATEGORY
    WHEN WS-BALANCE > 0
        MOVE 'BASIC' TO WS-CATEGORY
    WHEN OTHER
        MOVE 'OVERDRAWN' TO WS-CATEGORY
END-EVALUATE.

* Decision table (this is amazing)
EVALUATE WS-CUSTOMER-TYPE ALSO WS-BALANCE > 1000
    WHEN 'G' ALSO TRUE
        MOVE 0.25 TO WS-DISCOUNT
    WHEN 'G' ALSO FALSE
        MOVE 0.20 TO WS-DISCOUNT
    WHEN 'S' ALSO TRUE
        MOVE 0.15 TO WS-DISCOUNT
    WHEN 'S' ALSO FALSE
        MOVE 0.10 TO WS-DISCOUNT
    WHEN OTHER
        MOVE 0.05 TO WS-DISCOUNT
END-EVALUATE.
```

That last example is a decision table—a business logic pattern from the 1960s that's still brilliant today.

### Arithmetic Statements: Choose Your Verbosity

COBOL gives you options for math, from verbose to... less verbose:

```cobol
* The verbose way
ADD WS-PRICE TO WS-TOTAL.
SUBTRACT WS-DISCOUNT FROM WS-TOTAL.
MULTIPLY WS-QUANTITY BY WS-PRICE GIVING WS-LINE-TOTAL.
DIVIDE WS-TOTAL BY WS-COUNT GIVING WS-AVERAGE
    REMAINDER WS-LEFTOVER.

* The slightly less verbose way
ADD WS-PRICE WS-TAX GIVING WS-TOTAL.

* The modern way (COMPUTE)
COMPUTE WS-TOTAL = WS-PRICE + WS-TAX.
COMPUTE WS-AVERAGE = WS-TOTAL / WS-COUNT.
COMPUTE WS-DISCOUNT = WS-TOTAL * WS-DISCOUNT-RATE.

* With error handling
ADD WS-HUGE-NUMBER TO WS-COUNTER
    ON SIZE ERROR
        DISPLAY "Counter overflow!"
        MOVE 99999 TO WS-COUNTER
    NOT ON SIZE ERROR
        DISPLAY "Counter updated"
END-ADD.
```

The ON SIZE ERROR clause catches overflow—something JavaScript silently ignores until your numbers turn into `Infinity`.

### PERFORM Loops: Every Flavor Imaginable

COBOL's PERFORM loops are surprisingly flexible:

```cobol
* Simple repetition
PERFORM PRINT-HEADER 3 TIMES.

* Conditional loop
PERFORM PROCESS-RECORD
    UNTIL END-OF-FILE OR WS-ERROR-FLAG = 'Y'.

* With TEST BEFORE (default)
PERFORM UNTIL WS-COUNTER > 10
    DISPLAY WS-COUNTER
    ADD 1 TO WS-COUNTER
END-PERFORM.

* With TEST AFTER (do-while)
PERFORM WITH TEST AFTER
        UNTIL WS-RESPONSE = 'Y' OR WS-RESPONSE = 'N'
    DISPLAY "Continue (Y/N)? "
    ACCEPT WS-RESPONSE
END-PERFORM.

* Nested VARYING (nested for loops)
PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 10
    PERFORM VARYING WS-COL FROM 1 BY 1 UNTIL WS-COL > 10
        COMPUTE WS-CELL(WS-ROW, WS-COL) = WS-ROW * WS-COL
    END-PERFORM
END-PERFORM.

* The mind-bending VARYING with multiple variables
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
        AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 5
    DISPLAY "I=" WS-I " J=" WS-J
END-PERFORM.
```

That last one iterates through all combinations of I and J. It's like nested loops but in one statement.

### Real-World Example: Batch Processing

Here's a typical COBOL batch processing routine:

```cobol
       PROCEDURE DIVISION.

       MAIN-CONTROL.
           PERFORM INITIALIZATION.
           PERFORM PROCESS-ALL-RECORDS.
           PERFORM TERMINATION.
           STOP RUN.

       INITIALIZATION.
           DISPLAY "Starting batch process...".
           PERFORM OPEN-FILES.
           PERFORM READ-CONTROL-TOTALS.
           PERFORM INITIALIZE-COUNTERS.

       OPEN-FILES.
           OPEN INPUT TRANSACTION-FILE.
           OPEN I-O MASTER-FILE.
           OPEN OUTPUT REPORT-FILE.
           OPEN OUTPUT ERROR-FILE.

       INITIALIZE-COUNTERS.
           MOVE ZERO TO WS-RECORD-COUNT.
           MOVE ZERO TO WS-ERROR-COUNT.
           MOVE ZERO TO WS-UPDATE-COUNT.
           MOVE ZERO TO WS-INSERT-COUNT.
           SET NOT-END-OF-FILE TO TRUE.

       PROCESS-ALL-RECORDS.
           PERFORM READ-TRANSACTION.
           PERFORM PROCESS-ONE-TRANSACTION
                   UNTIL END-OF-FILE.

       READ-TRANSACTION.
           READ TRANSACTION-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
           END-READ.

       PROCESS-ONE-TRANSACTION.
           PERFORM VALIDATE-TRANSACTION.

           IF TRANSACTION-VALID
               PERFORM UPDATE-MASTER
           ELSE
               PERFORM WRITE-ERROR-RECORD
           END-IF.

           PERFORM READ-TRANSACTION.

       VALIDATE-TRANSACTION.
           SET TRANSACTION-VALID TO TRUE.

           IF TRANS-AMOUNT NOT NUMERIC
               SET TRANSACTION-INVALID TO TRUE
               MOVE "Non-numeric amount" TO WS-ERROR-MESSAGE
           END-IF.

           IF TRANSACTION-VALID
               IF TRANS-AMOUNT > 999999.99
                   SET TRANSACTION-INVALID TO TRUE
                   MOVE "Amount exceeds maximum" TO WS-ERROR-MESSAGE
               END-IF
           END-IF.

           IF TRANSACTION-VALID
               IF TRANS-DATE < WS-SYSTEM-DATE
                   SET TRANSACTION-INVALID TO TRUE
                   MOVE "Transaction date in past" TO WS-ERROR-MESSAGE
               END-IF
           END-IF.

       UPDATE-MASTER.
           MOVE TRANS-KEY TO MASTER-KEY.
           READ MASTER-FILE
               INVALID KEY
                   PERFORM INSERT-NEW-MASTER
               NOT INVALID KEY
                   PERFORM UPDATE-EXISTING-MASTER
           END-READ.

       UPDATE-EXISTING-MASTER.
           ADD TRANS-AMOUNT TO MASTER-BALANCE.
           MOVE WS-SYSTEM-DATE TO MASTER-LAST-UPDATE.
           REWRITE MASTER-RECORD.
           ADD 1 TO WS-UPDATE-COUNT.

       INSERT-NEW-MASTER.
           MOVE TRANS-KEY TO MASTER-KEY.
           MOVE TRANS-AMOUNT TO MASTER-BALANCE.
           MOVE WS-SYSTEM-DATE TO MASTER-CREATED.
           MOVE WS-SYSTEM-DATE TO MASTER-LAST-UPDATE.
           WRITE MASTER-RECORD.
           ADD 1 TO WS-INSERT-COUNT.

       WRITE-ERROR-RECORD.
           MOVE TRANSACTION-RECORD TO ERROR-TRANSACTION.
           MOVE WS-ERROR-MESSAGE TO ERROR-DESCRIPTION.
           WRITE ERROR-RECORD.
           ADD 1 TO WS-ERROR-COUNT.

       TERMINATION.
           PERFORM DISPLAY-TOTALS.
           PERFORM CLOSE-FILES.
           DISPLAY "Batch processing complete.".

       DISPLAY-TOTALS.
           DISPLAY "Records processed: " WS-RECORD-COUNT.
           DISPLAY "Records updated:   " WS-UPDATE-COUNT.
           DISPLAY "Records inserted:  " WS-INSERT-COUNT.
           DISPLAY "Errors found:      " WS-ERROR-COUNT.

       CLOSE-FILES.
           CLOSE TRANSACTION-FILE.
           CLOSE MASTER-FILE.
           CLOSE REPORT-FILE.
           CLOSE ERROR-FILE.
```

This is classic COBOL: read records, validate them, update a master file, handle errors, produce reports. It's been the pattern since 1960.

### Advanced PERFORM: The THRU Clause

PERFORM THRU executes all paragraphs in a range:

```cobol
       PERFORM PARA-A THRU PARA-C.

       PARA-A.
           DISPLAY "Executing A".

       PARA-B.
           DISPLAY "Executing B".

       PARA-C.
           DISPLAY "Executing C".

       * All three execute in sequence
```

This is controversial. It creates hidden dependencies—adding a paragraph between PARA-A and PARA-C changes behavior. Modern style avoids THRU except for exit paragraphs:

```cobol
       PERFORM PROCESS-CUSTOMER THRU PROCESS-CUSTOMER-EXIT.

       PROCESS-CUSTOMER.
           * Complex logic with multiple exit points
           IF condition-1
               GO TO PROCESS-CUSTOMER-EXIT
           END-IF.
           * More logic

       PROCESS-CUSTOMER-EXIT.
           EXIT.
```

### The EXIT Statement: COBOL's Pass

EXIT does... nothing. Literally:

```cobol
       DUMMY-PARAGRAPH.
           EXIT.
```

It's a placeholder, used with PERFORM THRU or as a target for GO TO. In modern COBOL, EXIT has variants:

```cobol
EXIT PROGRAM.     * Return from subprogram
EXIT PERFORM.     * Break out of PERFORM loop
EXIT SECTION.     * Leave current section
EXIT PARAGRAPH.   * Leave current paragraph
```

### Error Handling: The COBOL Way

COBOL doesn't have exceptions. It has conditions and status codes:

```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM RISKY-OPERATION.

           IF WS-STATUS NOT = '00'
               PERFORM ERROR-HANDLER
           END-IF.

       RISKY-OPERATION.
           CALL 'SUBPROG' USING WS-PARAMETERS
               ON EXCEPTION
                   MOVE '99' TO WS-STATUS
                   MOVE 'Subprogram not found' TO WS-ERROR-MSG
               NOT ON EXCEPTION
                   MOVE '00' TO WS-STATUS
           END-CALL.

       ERROR-HANDLER.
           EVALUATE WS-STATUS
               WHEN '10'
                   DISPLAY "Warning: " WS-ERROR-MSG
                   PERFORM CONTINUE-PROCESSING
               WHEN '20' THRU '89'
                   DISPLAY "Error: " WS-ERROR-MSG
                   PERFORM RECOVERY-ROUTINE
               WHEN '90' THRU '99'
                   DISPLAY "Fatal: " WS-ERROR-MSG
                   PERFORM EMERGENCY-SHUTDOWN
           END-EVALUATE.
```

No stack traces, no automatic propagation. You check status after every operation. It's tedious but explicit.

### COBOL's Forgotten Features

#### ALTER Statement (Deprecated)
```cobol
ALTER PARA-A TO PROCEED TO PARA-C.
* Now PARA-A jumps to PARA-C instead of continuing
* This is self-modifying code. Don't do this!
```

#### PERFORM VARYING with Complex Conditions
```cobol
PERFORM VARYING WS-I FROM 1 BY 1
        UNTIL WS-I > 100 OR WS-FOUND = 'Y'
        AFTER WS-J FROM WS-I BY 1
        UNTIL WS-J > 100 OR WS-FOUND = 'Y'
    IF WS-TABLE(WS-I) = WS-TABLE(WS-J)
        SET WS-FOUND TO TRUE
        DISPLAY "Duplicate at " WS-I " and " WS-J
    END-IF
END-PERFORM.
```

### Performance Considerations

1. **PERFORM vs GO TO**: PERFORM has overhead. In tight loops processing millions of records, GO TO can be faster. (But please don't.)

2. **Inline vs Out-of-line PERFORM**: Inline PERFORM avoids call overhead but increases code size.

3. **SEARCH vs Manual Loop**: COBOL's SEARCH statement (Chapter 6) uses binary search on sorted tables—much faster than manual loops.

4. **Arithmetic Statements**: COMPUTE is usually optimized better than individual ADD/SUBTRACT/MULTIPLY/DIVIDE.

### Common Patterns and Anti-Patterns

#### Pattern: Status Flag Control
```cobol
PERFORM UNTIL FINISHED OR ERROR-OCCURRED
    PERFORM READ-AND-VALIDATE
    IF VALID-RECORD
        PERFORM PROCESS-RECORD
    END-IF
END-PERFORM.
```

#### Anti-Pattern: GO TO Spaghetti
```cobol
       PARA-1.
           IF condition GO TO PARA-3.
           GO TO PARA-2.
       PARA-2.
           IF other-condition GO TO PARA-1.
           GO TO PARA-4.
       PARA-3.
           GO TO PARA-2.
       * Don't do this!
```

#### Pattern: Structured Error Handling
```cobol
PERFORM MAIN-PROCESS.
GO TO END-PROGRAM.

MAIN-PROCESS.
    PERFORM STEP-1.
    IF ERROR-FLAG = 'Y' EXIT PARAGRAPH.
    PERFORM STEP-2.
    IF ERROR-FLAG = 'Y' EXIT PARAGRAPH.
    PERFORM STEP-3.

END-PROGRAM.
    PERFORM CLEANUP.
    STOP RUN.
```

### The Art of COBOL Flow Control

Writing good PROCEDURE DIVISION code requires:

1. **Clear paragraph names**: `CALCULATE-DISCOUNT` not `PARA-1`
2. **Single responsibility**: Each paragraph does one thing
3. **Consistent error handling**: Check status after every operation
4. **Avoid deep nesting**: Use EVALUATE instead of nested IFs
5. **Document the why**: COBOL is self-documenting for what, not why

### Modern Enhancements

Recent COBOL standards added:

```cobol
* Local variables in paragraphs
PROCEDURE DIVISION.
    DECLARE WS-LOCAL PIC 9(5).  * Scoped to procedure

* Exception handling (COBOL 2002)
INVOKE object-reference "method-name"
    CATCHING EXCEPTION
        DISPLAY "Exception occurred"
END-INVOKE.

* Inline PERFORM with RETURNING
COMPUTE WS-RESULT = FUNCTION CALCULATE-TAX(WS-AMOUNT).
```

But again, production COBOL rarely uses these.

### What You've Learned

The PROCEDURE DIVISION is where COBOL shows its age—both the wisdom of experience and the scars of history. It's verbose, explicit, and occasionally maddening. But it's also clear, maintainable (when written well), and has processed trillions of dollars without the luxury of modern control structures.

You now understand:
- How COBOL organizes code (sections and paragraphs)
- PERFORM in all its varieties
- Why GO TO exists (and why to avoid it)
- EVALUATE for complex decisions
- How COBOL handles errors (manually, always manually)

### Next Chapter Preview

Chapter 5 dives into file handling—COBOL's bread and butter. You'll learn:
- Sequential, indexed, and relative file organizations
- The FILE-CONTROL paragraph
- READ, WRITE, REWRITE, DELETE operations
- Record locking and transaction control
- Why COBOL file handling is both primitive and bulletproof

Remember: COBOL was designed when "database" meant "filing cabinet" and "random access" was revolutionary. Understanding COBOL's file handling is understanding how data processing worked before SQL existed.

---

*Next Chapter: [Chapter 5: Working with Files and Records](chapter-05-files-and-records.md)*

*"In PROCEDURE DIVISION, every PERFORM is a promise, every GO TO is a threat, and every period is a potential disaster."*