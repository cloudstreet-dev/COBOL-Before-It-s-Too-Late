# Chapter 9: Database Access with COBOL
## When Files Weren't Enough Anymore

---

### From Files to Databases: The Evolution

In the beginning, COBOL had files. Then businesses wanted to query their data without writing a new program each time. Thus, databases were born, and COBOL had to adapt.

Today's COBOL programs connect to:
- **DB2** - IBM's relational database
- **IMS** - Hierarchical database (still huge in banking)
- **CICS** - Transaction processing with database access
- **Oracle, SQL Server** - Via middleware
- **Modern databases** - Through APIs and drivers

### Embedded SQL: The Marriage of COBOL and SQL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * SQL Communication Area
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       * Host variables (prefixed with :)
       01  WS-CUSTOMER-ID      PIC 9(10).
       01  WS-CUSTOMER-NAME    PIC X(30).
       01  WS-BALANCE          PIC S9(7)V99.
       01  WS-STATUS           PIC X.

       * Null indicators
       01  WS-BALANCE-IND      PIC S9(4) COMP.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CONNECT-TO-DB.
           PERFORM FETCH-CUSTOMER.
           PERFORM UPDATE-BALANCE.
           PERFORM DISCONNECT-DB.
           STOP RUN.

       CONNECT-TO-DB.
           EXEC SQL
               CONNECT TO CUSTDB
                   USER :WS-USERID
                   USING :WS-PASSWORD
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY "Connection failed: " SQLCODE
               STOP RUN
           END-IF.

       FETCH-CUSTOMER.
           MOVE 1234567890 TO WS-CUSTOMER-ID.

           EXEC SQL
               SELECT CUST_NAME, BALANCE, STATUS
               INTO :WS-CUSTOMER-NAME,
                    :WS-BALANCE:WS-BALANCE-IND,
                    :WS-STATUS
               FROM CUSTOMERS
               WHERE CUST_ID = :WS-CUSTOMER-ID
           END-EXEC.

           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY "Customer found: " WS-CUSTOMER-NAME
                   IF WS-BALANCE-IND < 0
                       DISPLAY "Balance is NULL"
                   ELSE
                       DISPLAY "Balance: " WS-BALANCE
                   END-IF
               WHEN 100
                   DISPLAY "Customer not found"
               WHEN OTHER
                   DISPLAY "SQL Error: " SQLCODE
                   PERFORM DISPLAY-ERROR
           END-EVALUATE.
```

### The SQLCA: Your Database Status Report

```cobol
       * SQL Communication Area (automatically included)
       01  SQLCA.
           05 SQLCAID         PIC X(8).
           05 SQLCABC         PIC S9(9) COMP.
           05 SQLCODE         PIC S9(9) COMP.
           05 SQLERRM.
              10 SQLERRML     PIC S9(4) COMP.
              10 SQLERRMC     PIC X(70).
           05 SQLERRP         PIC X(8).
           05 SQLERRD         OCCURS 6 PIC S9(9) COMP.
           05 SQLWARN.
              10 SQLWARN0     PIC X.
              10 SQLWARN1     PIC X.
              10 SQLWARN2     PIC X.
              10 SQLWARN3     PIC X.
              10 SQLWARN4     PIC X.
              10 SQLWARN5     PIC X.
              10 SQLWARN6     PIC X.
              10 SQLWARN7     PIC X.
           05 SQLEXT          PIC X(8).

       * Common SQLCODE values:
       * 0   = Success
       * 100 = No data found
       * < 0 = Error occurred
```

### Cursors: Processing Result Sets

```cobol
       * Declare cursor
       EXEC SQL
           DECLARE CUST_CURSOR CURSOR FOR
           SELECT CUST_ID, CUST_NAME, BALANCE
           FROM CUSTOMERS
           WHERE BALANCE > :WS-MIN-BALANCE
           ORDER BY BALANCE DESC
       END-EXEC.

       PROCESS-CUSTOMERS.
           * Open cursor
           EXEC SQL
               OPEN CUST_CURSOR
           END-EXEC.

           * Fetch rows
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CUST_CURSOR
                   INTO :WS-CUSTOMER-ID,
                        :WS-CUSTOMER-NAME,
                        :WS-BALANCE
               END-EXEC

               IF SQLCODE = 0
                   PERFORM PROCESS-ONE-CUSTOMER
               ELSE IF SQLCODE NOT = 100
                   PERFORM SQL-ERROR-HANDLER
               END-IF
           END-PERFORM.

           * Close cursor
           EXEC SQL
               CLOSE CUST_CURSOR
           END-EXEC.
```

### Transactions: COMMIT and ROLLBACK

```cobol
       TRANSFER-FUNDS.
           * Start transaction
           EXEC SQL
               BEGIN TRANSACTION
           END-EXEC.

           * Debit source account
           EXEC SQL
               UPDATE ACCOUNTS
               SET BALANCE = BALANCE - :WS-AMOUNT
               WHERE ACCOUNT_ID = :WS-FROM-ACCOUNT
           END-EXEC.

           IF SQLCODE NOT = 0
               PERFORM ROLLBACK-TRANS
               GO TO TRANSFER-EXIT
           END-IF.

           * Credit destination account
           EXEC SQL
               UPDATE ACCOUNTS
               SET BALANCE = BALANCE + :WS-AMOUNT
               WHERE ACCOUNT_ID = :WS-TO-ACCOUNT
           END-EXEC.

           IF SQLCODE NOT = 0
               PERFORM ROLLBACK-TRANS
           ELSE
               PERFORM COMMIT-TRANS
           END-IF.

       TRANSFER-EXIT.
           EXIT.

       COMMIT-TRANS.
           EXEC SQL
               COMMIT
           END-EXEC.
           DISPLAY "Transfer completed successfully".

       ROLLBACK-TRANS.
           EXEC SQL
               ROLLBACK
           END-EXEC.
           DISPLAY "Transfer failed - rolled back".
```

### Dynamic SQL: When You Don't Know the Query

```cobol
       01  WS-SQL-STATEMENT    PIC X(500).

       BUILD-DYNAMIC-QUERY.
           STRING "SELECT * FROM CUSTOMERS WHERE "
                  INTO WS-SQL-STATEMENT.

           IF WS-SEARCH-BY-NAME = 'Y'
               STRING WS-SQL-STATEMENT
                      " CUST_NAME LIKE '%"
                      WS-SEARCH-NAME
                      "%'"
                   INTO WS-SQL-STATEMENT
           ELSE
               STRING WS-SQL-STATEMENT
                      " CUST_ID = "
                      WS-SEARCH-ID
                   INTO WS-SQL-STATEMENT
           END-IF.

           * Prepare and execute
           EXEC SQL
               PREPARE STMT FROM :WS-SQL-STATEMENT
           END-EXEC.

           EXEC SQL
               EXECUTE STMT
           END-EXEC.

           * For SELECT statements, use cursor
           EXEC SQL
               DECLARE DYN_CURSOR CURSOR FOR STMT
           END-EXEC.
```

### CICS: COBOL's Transaction Processing

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICS-TRANS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGE          PIC X(80).

       LINKAGE SECTION.
       01  DFHCOMMAREA         PIC X(100).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS RECEIVE
               INTO(WS-MESSAGE)
               LENGTH(80)
           END-EXEC.

           * Read from VSAM file
           EXEC CICS READ
               FILE('CUSTFILE')
               INTO(CUSTOMER-RECORD)
               RIDFLD(WS-CUSTOMER-ID)
               RESP(WS-RESPONSE)
           END-EXEC.

           * Update record
           EXEC CICS REWRITE
               FILE('CUSTFILE')
               FROM(CUSTOMER-RECORD)
               RESP(WS-RESPONSE)
           END-EXEC.

           * Send response
           EXEC CICS SEND
               FROM(WS-MESSAGE)
               LENGTH(80)
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
```

### IMS: Hierarchical Database Access

```cobol
       * IMS database access
       CALL 'CBLTDLI' USING DLI-GU
                            PCB-NAME
                            IO-AREA
                            SSA-1.

       * DLI functions
       01  DLI-FUNCTIONS.
           05 DLI-GU           PIC X(4) VALUE 'GU  '.  * Get Unique
           05 DLI-GN           PIC X(4) VALUE 'GN  '.  * Get Next
           05 DLI-GNP          PIC X(4) VALUE 'GNP '.  * Get Next in Parent
           05 DLI-ISRT         PIC X(4) VALUE 'ISRT'.  * Insert
           05 DLI-DLET         PIC X(4) VALUE 'DLET'.  * Delete
           05 DLI-REPL         PIC X(4) VALUE 'REPL'.  * Replace

       * Segment Search Arguments
       01  SSA-1.
           05 SEGMENT-NAME     PIC X(8) VALUE 'CUSTOMER'.
           05 COMMAND          PIC X VALUE '('.
           05 FIELD-NAME       PIC X(8) VALUE 'CUSTID  '.
           05 OPERATOR         PIC XX VALUE ' ='.
           05 FIELD-VALUE      PIC X(10).
           05 FILLER           PIC X VALUE ')'.
```

### Stored Procedures: Calling Database Logic

```cobol
       * Call stored procedure
       EXEC SQL
           CALL CALCULATE_INTEREST(:WS-ACCOUNT-ID,
                                   :WS-RATE,
                                   :WS-PERIOD,
                                   :WS-NEW-BALANCE)
       END-EXEC.

       * Stored procedure with result set
       EXEC SQL
           DECLARE RESULT_SET CURSOR FOR
           CALL GET_CUSTOMER_ORDERS(:WS-CUSTOMER-ID)
       END-EXEC.

       EXEC SQL
           OPEN RESULT_SET
       END-EXEC.

       PERFORM UNTIL SQLCODE = 100
           EXEC SQL
               FETCH RESULT_SET
               INTO :WS-ORDER-ID,
                    :WS-ORDER-DATE,
                    :WS-ORDER-TOTAL
           END-EXEC

           IF SQLCODE = 0
               PERFORM PROCESS-ORDER
           END-IF
       END-PERFORM.
```

### Performance Optimization

```cobol
       * Use host variable arrays for bulk operations
       01  WS-CUSTOMER-ARRAY.
           05 WS-CUST-ENTRY OCCURS 100 TIMES.
              10 WS-CA-ID      PIC 9(10).
              10 WS-CA-NAME    PIC X(30).
              10 WS-CA-BALANCE PIC S9(7)V99.

       * Bulk fetch
       EXEC SQL
           FETCH CUST_CURSOR
           FOR 100 ROWS
           INTO :WS-CUSTOMER-ARRAY
       END-EXEC.

       * Bulk insert
       EXEC SQL
           INSERT INTO CUSTOMERS
           VALUES (:WS-CUSTOMER-ARRAY)
           FOR :WS-ROW-COUNT ROWS
       END-EXEC.

       * Connection pooling hint
       EXEC SQL
           SET CONNECTION POOLING = TRUE
       END-EXEC.
```

### Error Handling Best Practices

```cobol
       SQL-ERROR-HANDLER.
           DISPLAY "SQL Error occurred".
           DISPLAY "SQLCODE: " SQLCODE.
           DISPLAY "SQLERRM: " SQLERRMC.

           EVALUATE SQLCODE
               WHEN -803
                   DISPLAY "Duplicate key violation"
               WHEN -811
                   DISPLAY "Multiple rows returned for single-row query"
               WHEN -904
                   DISPLAY "Resource unavailable"
               WHEN -911
                   DISPLAY "Deadlock detected"
               WHEN OTHER
                   DISPLAY "Unknown error"
           END-EVALUATE.

           * Log to error table
           EXEC SQL
               INSERT INTO ERROR_LOG
               VALUES (CURRENT_TIMESTAMP,
                      :WS-PROGRAM-NAME,
                      :SQLCODE,
                      :SQLERRMC)
           END-EXEC.
```

### Modern Database Access: REST APIs

```cobol
       * Modern COBOL calling REST API
       01  WS-URL              PIC X(200).
       01  WS-REQUEST-BODY     PIC X(1000).
       01  WS-RESPONSE-BODY    PIC X(5000).
       01  WS-HTTP-STATUS      PIC 999.

       CALL-REST-API.
           MOVE "https://api.example.com/customers/12345"
               TO WS-URL.

           CALL 'HTTPGET' USING WS-URL
                                WS-RESPONSE-BODY
                                WS-HTTP-STATUS.

           IF WS-HTTP-STATUS = 200
               * Parse JSON response
               JSON PARSE WS-RESPONSE-BODY
                   INTO WS-CUSTOMER-RECORD
           ELSE
               DISPLAY "API Error: " WS-HTTP-STATUS
           END-IF.
```

### Database Migration Patterns

```cobol
       * Pattern 1: Dual-write during migration
       WRITE-CUSTOMER.
           * Write to legacy file
           WRITE CUSTOMER-FILE-REC.

           * Also write to new database
           EXEC SQL
               INSERT INTO CUSTOMERS
               VALUES (:WS-CUSTOMER-DATA)
           END-EXEC.

       * Pattern 2: Database fallback to file
       READ-CUSTOMER.
           EXEC SQL
               SELECT * INTO :WS-CUSTOMER
               FROM CUSTOMERS
               WHERE ID = :WS-ID
           END-EXEC.

           IF SQLCODE = 100
               * Try legacy file
               READ CUSTOMER-FILE
                   KEY IS CUST-ID
                   INVALID KEY
                       MOVE 'NOT-FOUND' TO WS-STATUS
               END-READ
           END-IF.
```

### Connection Management

```cobol
       * Connection with error handling
       CONNECT-WITH-RETRY.
           PERFORM VARYING WS-RETRY FROM 1 BY 1
                   UNTIL WS-RETRY > 3
                      OR SQLCODE = 0

               EXEC SQL
                   CONNECT TO :WS-DATABASE
                   USER :WS-USERID
                   USING :WS-PASSWORD
               END-EXEC

               IF SQLCODE NOT = 0
                   DISPLAY "Connection attempt " WS-RETRY " failed"
                   CALL 'CBL_OC_NANOSLEEP' USING WS-WAIT-TIME
               END-IF
           END-PERFORM.

           IF SQLCODE NOT = 0
               DISPLAY "Failed to connect after 3 attempts"
               STOP RUN
           END-IF.
```

### What You've Learned

COBOL's database access evolved from files to embedded SQL to modern APIs. The embedded SQL approach, while verbose, provides compile-time SQL checking and tight integration. CICS and IMS show how COBOL adapted to different database paradigms.

The key is understanding that COBOL treats database access like file I/O with extra steps. Every operation needs error checking, every result needs handling, and nothing is automatic. But this explicitness is why COBOL database programs are incredibly reliable.

---

*Next Chapter: [Chapter 10: Debugging and Testing COBOL](chapter-10-debugging-testing.md)*

*"In COBOL, NULL is not nothing—it's a negative indicator that something might be nothing."*