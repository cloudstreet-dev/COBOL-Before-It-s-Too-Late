# Chapter 12: Real-World Code Review Scenarios
## Finding the Bodies Buried in Production Code

---

### Welcome to Your First Day

It's Monday morning. You've been hired to maintain a COBOL system. Your manager hands you a printout (yes, paper) and says, "This program has been running for 30 years. We need to add a new field. Don't break anything."

Let's review some actual COBOL code you'll encounter and learn what to look for, what to fix, and what to leave alone (even if it hurts).

### Scenario 1: The Mysterious Calculation

You find this in production:

```cobol
       COMPUTE WS-TOTAL = WS-AMOUNT * 1.0825.
       IF WS-CUSTOMER-TYPE = 'S'
           COMPUTE WS-TOTAL = WS-AMOUNT * 1.0775
       ELSE IF WS-CUSTOMER-TYPE = 'G'
           COMPUTE WS-TOTAL = WS-AMOUNT * 1.065
       END-IF.

       * Special handling
       IF WS-CUSTOMER-ID = 00001234 OR 00005678 OR 00009012
           COMPUTE WS-TOTAL = WS-AMOUNT
       END-IF.
```

**What's Wrong:**
1. Magic numbers everywhere (what's 1.0825?)
2. Hard-coded customer IDs (why are these special?)
3. The first COMPUTE is always overwritten (wasteful)
4. That OR condition doesn't work like you think

**The Truth:**
```cobol
       * This is wrong! Each OR needs full condition
       IF WS-CUSTOMER-ID = 00001234 OR 00005678 OR 00009012

       * Should be:
       IF WS-CUSTOMER-ID = 00001234 OR
          WS-CUSTOMER-ID = 00005678 OR
          WS-CUSTOMER-ID = 00009012
```

**How to Fix:**
```cobol
       * Tax rates as of 2024-01-01 per Finance Dept memo #2024-001
       01  WS-TAX-RATES.
           05 WS-STANDARD-TAX     PIC V9999 VALUE .0825.
           05 WS-SILVER-TAX       PIC V9999 VALUE .0775.
           05 WS-GOLD-TAX         PIC V9999 VALUE .0650.

       * Tax-exempt customers (per Legal Dept ruling)
       01  WS-TAX-EXEMPT-LIST.
           05 WS-EXEMPT-ID OCCURS 3 TIMES PIC 9(8).

       CALCULATE-TAX.
           * Check for tax exemption first
           PERFORM CHECK-TAX-EXEMPT.

           IF TAX-EXEMPT
               MOVE WS-AMOUNT TO WS-TOTAL
           ELSE
               PERFORM APPLY-TAX-RATE
           END-IF.

       APPLY-TAX-RATE.
           EVALUATE WS-CUSTOMER-TYPE
               WHEN 'G'
                   COMPUTE WS-TOTAL = WS-AMOUNT * (1 + WS-GOLD-TAX)
               WHEN 'S'
                   COMPUTE WS-TOTAL = WS-AMOUNT * (1 + WS-SILVER-TAX)
               WHEN OTHER
                   COMPUTE WS-TOTAL = WS-AMOUNT * (1 + WS-STANDARD-TAX)
           END-EVALUATE.
```

### Scenario 2: The File Handler From Hell

```cobol
       READ-CUSTOMER-FILE.
           READ CUSTFILE.
           IF WS-STATUS = '00'
               MOVE CUST-REC TO WS-CUSTOMER
           ELSE
               DISPLAY "ERROR"
               STOP RUN
           END-IF.
```

**What's Wrong:**
1. No file status checking on OPEN
2. Generic error message
3. STOP RUN in the middle of processing
4. No handling for end-of-file
5. No cleanup on error

**Better Version:**
```cobol
       READ-CUSTOMER-SAFELY.
           READ CUSTFILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   MOVE CUST-REC TO WS-CUSTOMER
                   ADD 1 TO WS-RECORD-COUNT
           END-READ.

           IF WS-FILE-STATUS NOT = '00' AND
              WS-FILE-STATUS NOT = '10'
               DISPLAY "File error reading CUSTFILE"
               DISPLAY "Status: " WS-FILE-STATUS
               DISPLAY "Record: " WS-RECORD-COUNT
               PERFORM CLOSE-ALL-FILES
               MOVE 16 TO RETURN-CODE
               GOBACK
           END-IF.
```

### Scenario 3: The Y2K "Fix"

```cobol
       * Y2K fix added 12/31/1999 by Bob
       IF WS-YEAR < 50
           ADD 2000 TO WS-YEAR
       ELSE
           ADD 1900 TO WS-YEAR
       END-IF.
```

**The Problem:** It's 2025. This "fix" now thinks 2025 is 1925.

**The Real Fix:**
```cobol
       * Date windowing updated for 2020+ dates
       * Window: 1950-2049
       01  WS-CENTURY-WINDOW   PIC 99 VALUE 50.

       DETERMINE-CENTURY.
           IF WS-YEAR-2-DIGIT < WS-CENTURY-WINDOW
               COMPUTE WS-YEAR-4-DIGIT = 2000 + WS-YEAR-2-DIGIT
           ELSE
               COMPUTE WS-YEAR-4-DIGIT = 1900 + WS-YEAR-2-DIGIT
           END-IF.

       * Better: Use 4-digit years everywhere
       * Best: Use DATE functions
       MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
```

### Scenario 4: The GOTO Maze

```cobol
       PROCESS-TRANSACTION.
           IF TRANS-TYPE = 'A'
               GO TO ADD-TRANS.
           IF TRANS-TYPE = 'U'
               GO TO UPDATE-TRANS.
           IF TRANS-TYPE = 'D'
               GO TO DELETE-TRANS.
           GO TO ERROR-TRANS.

       ADD-TRANS.
           ADD 1 TO ADD-COUNT.
           GO TO WRITE-TRANS.

       UPDATE-TRANS.
           ADD 1 TO UPDATE-COUNT.
           GO TO WRITE-TRANS.

       DELETE-TRANS.
           ADD 1 TO DELETE-COUNT.

       WRITE-TRANS.
           WRITE TRANS-RECORD.
           GO TO END-TRANS.

       ERROR-TRANS.
           ADD 1 TO ERROR-COUNT.

       END-TRANS.
           EXIT.
```

**Why It's Bad:** Following the logic requires a map and compass.

**The Refactor:**
```cobol
       PROCESS-TRANSACTION.
           EVALUATE TRANS-TYPE
               WHEN 'A'
                   ADD 1 TO ADD-COUNT
                   PERFORM WRITE-TRANSACTION
               WHEN 'U'
                   ADD 1 TO UPDATE-COUNT
                   PERFORM WRITE-TRANSACTION
               WHEN 'D'
                   ADD 1 TO DELETE-COUNT
                   PERFORM WRITE-TRANSACTION
               WHEN OTHER
                   ADD 1 TO ERROR-COUNT
                   PERFORM LOG-ERROR
           END-EVALUATE.
```

### Scenario 5: The Decimal Disaster

```cobol
       01  WS-PRICE           PIC 9(5)V99.
       01  WS-QTY             PIC 999.
       01  WS-TOTAL           PIC 9(7)V99.

       COMPUTE WS-TOTAL = WS-PRICE * WS-QTY / 100.
```

**The Bug:** Why divide by 100? Because someone forgot V means implied decimal!

**The Investigation:**
```cobol
       * If WS-PRICE = 12345 (representing 123.45)
       * And WS-QTY = 10
       * WS-TOTAL = 123.45 * 10 = 1234.50 (correct)
       * But divided by 100 = 12.34 (wrong!)
```

**The Fix:**
```cobol
       * Remove the mysterious /100
       COMPUTE WS-TOTAL = WS-PRICE * WS-QTY.

       * Add documentation
       * Note: PIC 9(5)V99 stores 123.45 as 12345
       * The V (implied decimal) handles decimal alignment
```

### Scenario 6: The Performance Killer

```cobol
       FIND-CUSTOMER.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > 1000000

               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-NOT-FOUND
                   NOT AT END
                       IF CUST-ID = WS-SEARCH-ID
                           MOVE CUST-REC TO WS-FOUND-REC
                           MOVE 1000001 TO WS-I
                       END-IF
               END-READ
           END-PERFORM.
```

**The Problem:** Sequential search through possibly 1 million records!

**The Solution:**
```cobol
       * Use indexed file organization
       READ CUSTOMER-FILE
           KEY IS CUST-ID
           INVALID KEY
               SET CUSTOMER-NOT-FOUND TO TRUE
           NOT INVALID KEY
               MOVE CUST-REC TO WS-FOUND-REC
               SET CUSTOMER-FOUND TO TRUE
       END-READ.

       * Or at least exit the loop properly
       PERFORM UNTIL WS-I > 1000000 OR CUSTOMER-FOUND
           * ... search logic
       END-PERFORM.
```

### Scenario 7: The Copy-Paste Catastrophe

```cobol
       CALCULATE-JANUARY.
           COMPUTE JAN-TOTAL = JAN-SALES * 1.08.
           ADD JAN-TOTAL TO Q1-TOTAL.

       CALCULATE-FEBRUARY.
           COMPUTE FEB-TOTAL = JAN-SALES * 1.08.  * <-- WRONG!
           ADD FEB-TOTAL TO Q1-TOTAL.

       CALCULATE-MARCH.
           COMPUTE MAR-TOTAL = JAN-SALES * 1.08.  * <-- WRONG!
           ADD MAR-TOTAL TO Q1-TOTAL.
```

**The Pattern:** Copy-paste error. February and March are using January's sales!

**The Prevention:**
```cobol
       * Use arrays and loops
       01  WS-MONTHLY-DATA.
           05 WS-MONTH-TABLE OCCURS 12 TIMES.
              10 WS-MONTH-SALES    PIC 9(7)V99.
              10 WS-MONTH-TOTAL    PIC 9(7)V99.

       01  WS-TAX-RATE            PIC V999 VALUE .080.

       CALCULATE-ALL-MONTHS.
           PERFORM VARYING WS-MONTH FROM 1 BY 1
                   UNTIL WS-MONTH > 12
               COMPUTE WS-MONTH-TOTAL(WS-MONTH) =
                       WS-MONTH-SALES(WS-MONTH) * (1 + WS-TAX-RATE)

               * Add to appropriate quarter
               EVALUATE WS-MONTH
                   WHEN 1 THRU 3
                       ADD WS-MONTH-TOTAL(WS-MONTH) TO WS-Q1-TOTAL
                   WHEN 4 THRU 6
                       ADD WS-MONTH-TOTAL(WS-MONTH) TO WS-Q2-TOTAL
                   WHEN 7 THRU 9
                       ADD WS-MONTH-TOTAL(WS-MONTH) TO WS-Q3-TOTAL
                   WHEN 10 THRU 12
                       ADD WS-MONTH-TOTAL(WS-MONTH) TO WS-Q4-TOTAL
               END-EVALUATE
           END-PERFORM.
```

### Scenario 8: The Buffer Overflow Waiting to Happen

```cobol
       01  WS-INPUT           PIC X(50).
       01  WS-OUTPUT          PIC X(30).

       MOVE WS-INPUT TO WS-OUTPUT.
```

**What Happens:** If WS-INPUT contains 50 characters, moving to WS-OUTPUT truncates at 30. Silent data loss!

**The Safe Approach:**
```cobol
       01  WS-INPUT           PIC X(50).
       01  WS-OUTPUT          PIC X(30).
       01  WS-INPUT-LENGTH    PIC 99.

       CHECK-AND-MOVE.
           * Get actual length
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT))
               TO WS-INPUT-LENGTH.

           IF WS-INPUT-LENGTH > 30
               DISPLAY "Warning: Input truncated from "
                       WS-INPUT-LENGTH " to 30 characters"
               MOVE WS-INPUT(1:30) TO WS-OUTPUT
           ELSE
               MOVE WS-INPUT TO WS-OUTPUT
           END-IF.
```

### Scenario 9: The Database Disaster

```cobol
       UPDATE-ALL-SALARIES.
           EXEC SQL
               UPDATE EMPLOYEES
               SET SALARY = SALARY * 1.05
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "Salaries updated"
           END-IF.
```

**The Horror:** No WHERE clause! Everyone gets a 5% raise! (The CEO is happy, CFO is not)

**The Fix:**
```cobol
       UPDATE-ELIGIBLE-SALARIES.
           * First, verify the count
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-UPDATE-COUNT
               FROM EMPLOYEES
               WHERE ELIGIBLE_FOR_RAISE = 'Y'
                 AND REVIEW_DATE <= CURRENT_DATE
           END-EXEC.

           DISPLAY "About to update " WS-UPDATE-COUNT " employees".
           DISPLAY "Continue? (Y/N)".
           ACCEPT WS-CONTINUE.

           IF WS-CONTINUE = 'Y'
               EXEC SQL
                   UPDATE EMPLOYEES
                   SET SALARY = SALARY * 1.05,
                       LAST_RAISE_DATE = CURRENT_DATE
                   WHERE ELIGIBLE_FOR_RAISE = 'Y'
                     AND REVIEW_DATE <= CURRENT_DATE
               END-EXEC

               IF SQLCODE = 0
                   DISPLAY "Updated " SQLERRD(3) " employee salaries"
                   EXEC SQL COMMIT END-EXEC
               ELSE
                   DISPLAY "Update failed, rolling back"
                   EXEC SQL ROLLBACK END-EXEC
               END-IF
           END-IF.
```

### Scenario 10: The Comment Archaeology

```cobol
       * Changed by Bob 1982 - Add customer type
       * Modified by Sarah 1987 - Fix customer type
       * Updated by Jim 1993 - Customer type still broken
       * Patched by Maria 1998 - Don't touch this!
       * Y2K fix by Dave 1999 - Works now
       * Broken again by unknown 2003
       * Fixed properly by Ahmed 2010 - DO NOT CHANGE
       * Emergency fix by Lisa 2015 - Ahmed was wrong
       * Reverted by Tom 2018 - Lisa's fix broke production
       * Final fix by Kim 2020 - See document #2020-147
       * Not actually fixed - Carlos 2023

       IF WS-CUSTOMER-TYPE = 'G' OR 'S' OR 'B'
           MOVE 'Y' TO WS-VALID-TYPE
       ELSE
           MOVE 'N' TO WS-VALID-TYPE
       END-IF.
```

**The Lesson:** 40 years of fixes and it's still wrong (OR condition bug). Sometimes starting fresh is better than another patch.

### Code Review Checklist for COBOL

#### Data Division
- [ ] Are all variables initialized?
- [ ] Do PIC clauses match intended use?
- [ ] Are decimal points (V) correctly placed?
- [ ] Are COMP fields used appropriately?
- [ ] Are table sizes sufficient?
- [ ] Are 88-level conditions comprehensive?

#### Procedure Division
- [ ] Is every file operation checked for status?
- [ ] Are all possible SQLCODE values handled?
- [ ] Do all loops have exit conditions?
- [ ] Are array subscripts within bounds?
- [ ] Is every PERFORM range correct?
- [ ] Are all GO TOs justified?

#### Business Logic
- [ ] Are calculations financially correct?
- [ ] Is decimal arithmetic precise?
- [ ] Are date calculations Y2K+ compliant?
- [ ] Are all business rules documented?
- [ ] Are edge cases handled?

#### Performance
- [ ] Are files opened/closed appropriately?
- [ ] Are database cursors closed?
- [ ] Is memory usage reasonable?
- [ ] Are searches optimized?
- [ ] Are bulk operations batched?

### The Golden Rules of COBOL Code Review

1. **If it's been running for 20 years, understand WHY before changing**
2. **That weird IF statement might be handling a critical edge case**
3. **Test with production-like data volumes**
4. **Check the PICTURE clauses match the actual data**
5. **Verify decimal point alignment in calculations**
6. **Never trust a comment that's older than you**
7. **Always check file status after I/O**
8. **Remember: tables are 1-indexed**
9. **When in doubt, ADD MORE DISPLAY STATEMENTS**
10. **The bug is probably in the last place you'd look: the ENVIRONMENT DIVISION**

### Your First Production Fix

Remember that field your manager wanted you to add? Here's how to do it safely:

```cobol
       * Step 1: Understand the current structure
       * Step 2: Find ALL copybooks that define this record
       * Step 3: Check ALL programs that use this file
       * Step 4: Add field at END of record (never middle!)
       * Step 5: Initialize new field properly
       * Step 6: Test with production-volume data
       * Step 7: Have a rollback plan
       * Step 8: Deploy during maintenance window
       * Step 9: Monitor for a week
       * Step 10: Document everything
```

### What You've Learned

Code review in COBOL isn't just about finding bugs—it's about archaeology, psychology, and forensics. You're not just reading code; you're reading decades of business decisions, technical compromises, and 3 AM emergency fixes.

The code you review today will be running when you retire. The fix you implement tonight will be processing someone's mortgage payment in 2050. No pressure.

---

## Conclusion: You're Ready

You now know enough COBOL to be dangerous. You can read that production code, make that critical fix, and perform that code review. You understand why COBOL is the way it is, and more importantly, why it's still here.

Remember:
- COBOL isn't bad, it's different
- That 40-year-old code is battle-tested
- Verbose is better than clever
- When in doubt, DISPLAY it out
- The mainframe always wins

Welcome to the exclusive club of COBOL programmers. The pay is good, the systems are critical, and the code is older than your parents.

Now go forth and PERFORM UNTIL RETIRED.

---

*"COBOL: It's not a career, it's a calling. A very well-paid calling."*

**THE END**

*STOP RUN.*