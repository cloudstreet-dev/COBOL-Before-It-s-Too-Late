# Chapter 10: Debugging and Testing COBOL
## Finding Bugs in Code Older Than You Are

---

### The DISPLAY Statement: Printf Debugging Since 1959

When all else fails, DISPLAY is there for you:

```cobol
       PROCEDURE DIVISION.
       DEBUG-NIGHTMARE.
           DISPLAY "Entering DEBUG-NIGHTMARE".
           DISPLAY "WS-COUNTER = " WS-COUNTER.

           PERFORM COMPLEX-CALCULATION.

           DISPLAY "After calculation:".
           DISPLAY "  Result = " WS-RESULT.
           DISPLAY "  Status = " WS-STATUS.

           IF WS-RESULT > 1000
               DISPLAY "!!! UNEXPECTED RESULT !!!".
```

Yes, this is primitive. No, there isn't always a better way.

### The READY TRACE and RESET TRACE

```cobol
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           READY TRACE.  * Start tracing execution

           PERFORM SOME-LOGIC.
           PERFORM MORE-LOGIC.

           RESET TRACE.  * Stop tracing

       * Output shows every paragraph entered
       * MAIN-LOGIC
       * SOME-LOGIC
       * MORE-LOGIC
```

Warning: READY TRACE can produce MASSIVE output in production. Use carefully.

### Debugging with Compiler Directives

```cobol
      *SET SOURCEFORMAT"FREE"
       >>SOURCE FORMAT IS FREE
       >>D DISPLAY "Debug mode active"

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEBUG-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       >>D 01 WS-DEBUG-FLAG PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
       >>D     DISPLAY "Starting main logic"
       >>D     DISPLAY "Debug flag = " WS-DEBUG-FLAG

           PERFORM BUSINESS-LOGIC.

       >>D     DISPLAY "Ending main logic"
           STOP RUN.
```

The >>D lines only compile when debugging is enabled.

### DECLARATIVES: COBOL's Exception Handlers

```cobol
       PROCEDURE DIVISION.
       DECLARATIVES.
       FILE-ERROR-HANDLER SECTION.
           USE AFTER ERROR PROCEDURE ON CUSTOMER-FILE.

       FILE-ERROR-PARA.
           DISPLAY "File error occurred!".
           DISPLAY "File Status: " WS-FILE-STATUS.

           EVALUATE WS-FILE-STATUS
               WHEN "23"
                   DISPLAY "Record not found"
               WHEN "35"
                   DISPLAY "File not found"
               WHEN "39"
                   DISPLAY "File attributes mismatch"
               WHEN OTHER
                   DISPLAY "Unknown file error"
           END-EVALUATE.

       END DECLARATIVES.

       MAIN-SECTION SECTION.
       MAIN-LOGIC.
           OPEN INPUT CUSTOMER-FILE.
           * If error, FILE-ERROR-HANDLER is called automatically
```

### Debugging Data Issues

```cobol
       * Common data debugging technique
       01  WS-HEX-DISPLAY.
           05 FILLER OCCURS 20 TIMES.
              10 WS-HEX-BYTE  PIC XX.

       DISPLAY-IN-HEX.
           * Show actual hex values of data
           MOVE WS-PROBLEM-FIELD TO WS-HEX-DISPLAY.
           DISPLAY "Hex dump: " WS-HEX-DISPLAY.

       * Check for non-printable characters
       INSPECT-DATA.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LENGTH OF WS-DATA

               IF WS-DATA(WS-I:1) < SPACE
                  OR WS-DATA(WS-I:1) > "~"
                   DISPLAY "Non-printable at position " WS-I
                           ": " FUNCTION ORD(WS-DATA(WS-I:1))
               END-IF
           END-PERFORM.
```

### Interactive Debugging with COBDB

```cobol
       * Add debugging lines
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG DEBUGGING MODE.

       * In PROCEDURE DIVISION
       >>D PERFORM BREAKPOINT

       BREAKPOINT.
           DISPLAY "Breakpoint reached".
           DISPLAY "Press ENTER to continue".
           ACCEPT WS-DUMMY.

           DISPLAY "Current values:".
           DISPLAY "  Customer ID: " WS-CUSTOMER-ID.
           DISPLAY "  Balance: " WS-BALANCE.
           DISPLAY "  Status: " WS-STATUS.
```

### Unit Testing in COBOL (Yes, It's Possible)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-SUITE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-COUNTERS.
           05 WS-TESTS-RUN     PIC 999 VALUE ZERO.
           05 WS-TESTS-PASSED  PIC 999 VALUE ZERO.
           05 WS-TESTS-FAILED  PIC 999 VALUE ZERO.

       01  WS-EXPECTED        PIC 9(9)V99.
       01  WS-ACTUAL          PIC 9(9)V99.

       PROCEDURE DIVISION.
       RUN-ALL-TESTS.
           DISPLAY "Starting test suite...".
           DISPLAY " ".

           PERFORM TEST-CALCULATE-TAX.
           PERFORM TEST-VALIDATE-SSN.
           PERFORM TEST-FORMAT-CURRENCY.

           DISPLAY " ".
           DISPLAY "Test Results:".
           DISPLAY "  Tests run: " WS-TESTS-RUN.
           DISPLAY "  Passed: " WS-TESTS-PASSED.
           DISPLAY "  Failed: " WS-TESTS-FAILED.

           IF WS-TESTS-FAILED > 0
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

           STOP RUN.

       TEST-CALCULATE-TAX.
           ADD 1 TO WS-TESTS-RUN.

           * Test case 1: Normal calculation
           MOVE 100.00 TO WS-AMOUNT.
           CALL 'CALCTAX' USING WS-AMOUNT WS-ACTUAL.
           MOVE 108.50 TO WS-EXPECTED.

           IF WS-ACTUAL = WS-EXPECTED
               ADD 1 TO WS-TESTS-PASSED
               DISPLAY "✓ TEST-CALCULATE-TAX passed"
           ELSE
               ADD 1 TO WS-TESTS-FAILED
               DISPLAY "✗ TEST-CALCULATE-TAX failed"
               DISPLAY "  Expected: " WS-EXPECTED
               DISPLAY "  Actual: " WS-ACTUAL
           END-IF.

       TEST-VALIDATE-SSN.
           ADD 1 TO WS-TESTS-RUN.

           * Test valid SSN
           MOVE "123-45-6789" TO WS-SSN.
           CALL 'VALIDATE' USING 'SSN' WS-SSN WS-RETURN-CODE.

           IF WS-RETURN-CODE = '00'
               ADD 1 TO WS-TESTS-PASSED
               DISPLAY "✓ TEST-VALIDATE-SSN passed"
           ELSE
               ADD 1 TO WS-TESTS-FAILED
               DISPLAY "✗ TEST-VALIDATE-SSN failed"
           END-IF.
```

### Testing File Operations

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FILE-OPS.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT TEST-FILE ASSIGN TO "TEST.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TEST-FILE.
       01  TEST-RECORD        PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS     PIC XX.
       01  WS-TEST-DATA       PIC X(80) VALUE "TEST DATA".

       PROCEDURE DIVISION.
       TEST-FILE-OPERATIONS.
           * Setup - Create test file
           PERFORM SETUP-TEST-FILE.

           * Test - Read from file
           PERFORM TEST-READ-OPERATION.

           * Teardown - Clean up
           PERFORM CLEANUP-TEST-FILE.

       SETUP-TEST-FILE.
           OPEN OUTPUT TEST-FILE.
           MOVE WS-TEST-DATA TO TEST-RECORD.
           WRITE TEST-RECORD.
           CLOSE TEST-FILE.

       TEST-READ-OPERATION.
           OPEN INPUT TEST-FILE.
           READ TEST-FILE.

           IF TEST-RECORD = WS-TEST-DATA
               DISPLAY "✓ File read test passed"
           ELSE
               DISPLAY "✗ File read test failed"
           END-IF.

           CLOSE TEST-FILE.

       CLEANUP-TEST-FILE.
           CALL 'CBL_DELETE_FILE' USING "TEST.DAT".
```

### Memory Dump Analysis

```cobol
       * Dump working storage for analysis
       01  WS-DUMP-AREA       PIC X(1000).
       01  WS-DUMP-POINTER    POINTER.

       DUMP-MEMORY.
           SET WS-DUMP-POINTER TO ADDRESS OF WS-PROBLEM-AREA.

           CALL 'CBL_MEM_DUMP' USING WS-DUMP-POINTER
                                     WS-DUMP-AREA
                                     1000.

           * Write to debug file
           OPEN OUTPUT DEBUG-FILE.
           WRITE DEBUG-RECORD FROM WS-DUMP-AREA.
           CLOSE DEBUG-FILE.
```

### Performance Profiling

```cobol
       01  WS-TIMER-FIELDS.
           05 WS-START-TIME   PIC 9(8).
           05 WS-END-TIME     PIC 9(8).
           05 WS-ELAPSED      PIC 9(8).

       PROCEDURE DIVISION.
       PERFORMANCE-TEST.
           * Get start time
           ACCEPT WS-START-TIME FROM TIME.

           * Code to profile
           PERFORM COMPLEX-PROCESS.

           * Get end time
           ACCEPT WS-END-TIME FROM TIME.

           * Calculate elapsed
           COMPUTE WS-ELAPSED = WS-END-TIME - WS-START-TIME.

           DISPLAY "Elapsed time: " WS-ELAPSED " hundredths of seconds".

           * More detailed timing with CBL routine
           CALL 'CBL_GET_CSR_POS' USING WS-START-TIME.
           PERFORM COMPLEX-PROCESS.
           CALL 'CBL_GET_CSR_POS' USING WS-END-TIME.
```

### Debugging Production Issues

```cobol
       * Production debugging without stopping the system
       01  WS-DEBUG-SWITCH    PIC X VALUE 'N'.
           88 DEBUG-ON        VALUE 'Y'.
           88 DEBUG-OFF       VALUE 'N'.

       01  WS-DEBUG-FILE-NAME PIC X(20) VALUE 'DEBUG.LOG'.

       PROCEDURE DIVISION.
       CHECK-DEBUG-MODE.
           * Read debug flag from environment
           ACCEPT WS-DEBUG-SWITCH FROM ENVIRONMENT "COBOL_DEBUG".

       MAIN-PROCESS.
           IF DEBUG-ON
               PERFORM OPEN-DEBUG-FILE
           END-IF.

           PERFORM BUSINESS-LOGIC.

           IF DEBUG-ON
               PERFORM CLOSE-DEBUG-FILE
           END-IF.

       BUSINESS-LOGIC.
           IF DEBUG-ON
               STRING "Processing record: " WS-RECORD-ID
                   DELIMITED BY SIZE INTO DEBUG-LINE
               WRITE DEBUG-RECORD
           END-IF.
```

### Assertion-Style Checking

```cobol
       * Assert macro simulation
       01  WS-ASSERT-MESSAGE  PIC X(80).

       ASSERT-EQUALS.
           IF WS-EXPECTED NOT = WS-ACTUAL
               STRING "Assertion failed: Expected "
                      WS-EXPECTED
                      " but got "
                      WS-ACTUAL
                   INTO WS-ASSERT-MESSAGE
               DISPLAY WS-ASSERT-MESSAGE
               STOP RUN
           END-IF.

       ASSERT-NOT-NULL.
           IF WS-VALUE = SPACES OR LOW-VALUES
               DISPLAY "Assertion failed: Value is null"
               STOP RUN
           END-IF.

       ASSERT-IN-RANGE.
           IF WS-VALUE < WS-MIN OR WS-VALUE > WS-MAX
               DISPLAY "Assertion failed: Value out of range"
               STOP RUN
           END-IF.
```

### Test Data Generation

```cobol
       * Generate test data
       01  WS-RANDOM-SEED     PIC 9(9) COMP.
       01  WS-RANDOM-NUMBER   PIC 9(9) COMP.

       GENERATE-TEST-DATA.
           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-RANDOM-SEED.

           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > 1000

               * Generate random customer ID
               COMPUTE WS-RANDOM-NUMBER =
                   FUNCTION RANDOM(WS-RANDOM-SEED) * 999999999
               MOVE WS-RANDOM-NUMBER TO TEST-CUSTOMER-ID

               * Generate random name
               PERFORM GENERATE-RANDOM-NAME

               * Generate random balance
               COMPUTE TEST-BALANCE =
                   FUNCTION RANDOM() * 100000

               WRITE TEST-RECORD
           END-PERFORM.
```

### Integration Testing

```cobol
       * Test complete workflow
       INTEGRATION-TEST.
           DISPLAY "Starting integration test...".

           * Step 1: Create test customer
           PERFORM CREATE-TEST-CUSTOMER.
           IF WS-STATUS NOT = 'OK'
               DISPLAY "Failed to create customer"
               GO TO TEST-FAILED
           END-IF.

           * Step 2: Update customer
           PERFORM UPDATE-TEST-CUSTOMER.
           IF WS-STATUS NOT = 'OK'
               DISPLAY "Failed to update customer"
               GO TO TEST-FAILED
           END-IF.

           * Step 3: Verify changes
           PERFORM VERIFY-CUSTOMER-CHANGES.
           IF WS-STATUS NOT = 'OK'
               DISPLAY "Verification failed"
               GO TO TEST-FAILED
           END-IF.

           * Step 4: Cleanup
           PERFORM DELETE-TEST-CUSTOMER.

           DISPLAY "Integration test passed!".
           GO TO TEST-END.

       TEST-FAILED.
           DISPLAY "Integration test FAILED!".
           PERFORM DELETE-TEST-CUSTOMER.

       TEST-END.
           EXIT.
```

### Common Debugging Patterns

#### Pattern 1: Binary Search for Bugs
```cobol
       * Disable half the code to isolate problem
       >>D MOVE 'Y' TO WS-SKIP-SECTION-A.

       IF WS-SKIP-SECTION-A NOT = 'Y'
           PERFORM SECTION-A
       END-IF.
```

#### Pattern 2: Data Snapshot
```cobol
       * Save state before problem area
       MOVE WS-CRITICAL-DATA TO WS-SAVE-DATA.
       PERFORM RISKY-OPERATION.
       IF WS-CRITICAL-DATA NOT = WS-SAVE-DATA
           DISPLAY "Data corruption detected!"
       END-IF.
```

#### Pattern 3: Trace Table
```cobol
       01  WS-TRACE-TABLE.
           05 WS-TRACE-ENTRY OCCURS 100 TIMES.
              10 WS-TRACE-TIME  PIC 9(8).
              10 WS-TRACE-LOC   PIC X(30).
              10 WS-TRACE-DATA  PIC X(50).

       01  WS-TRACE-INDEX     PIC 999 VALUE 1.

       ADD-TRACE.
           IF WS-TRACE-INDEX <= 100
               ACCEPT WS-TRACE-TIME(WS-TRACE-INDEX) FROM TIME
               MOVE WS-LOCATION TO WS-TRACE-LOC(WS-TRACE-INDEX)
               MOVE WS-DATA TO WS-TRACE-DATA(WS-TRACE-INDEX)
               ADD 1 TO WS-TRACE-INDEX
           END-IF.
```

### Debugging Checklist

1. **Check file status after EVERY operation**
2. **Verify data types match in MOVE statements**
3. **Look for off-by-one errors in table access**
4. **Check for spaces in numeric fields**
5. **Verify PERFORM ranges with THRU**
6. **Check for missing END- statements**
7. **Verify decimal point alignment**
8. **Look for uninitialized variables**
9. **Check SQLCODE after database operations**
10. **Verify array bounds (remember: 1-indexed!)**

### What You've Learned

Debugging COBOL is archaeology. You're digging through layers of code, some written before you were born, looking for why a number is suddenly negative or why the report has extra spaces. The tools are primitive, but the approach is systematic: isolate, reproduce, fix, verify.

Testing COBOL requires creativity. Without modern frameworks, you build your own test harnesses. Without debuggers, you instrument your code. It's manual, it's tedious, but it works.

---

*Next Chapter: [Chapter 11: Modernizing Legacy COBOL](chapter-11-modernizing.md)*

*"In COBOL debugging, DISPLAY is your printf, your console.log, and your therapist."*