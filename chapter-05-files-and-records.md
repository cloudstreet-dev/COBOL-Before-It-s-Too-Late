# Chapter 5: Working with Files and Records
## COBOL's Filing Cabinet Metaphor Come to Life

---

### Before Databases, There Were Files

In 1959, there was no SQL, no MongoDB, no Redis. There were magnetic tapes and eventually disk drives. COBOL was built for this world, and its file handling shows it. Every file is a sequence of records. Every record is a fixed structure. It's primitive, inflexible, and absolutely bulletproof.

Modern developers: Imagine if every database table was actually a CSV file, but with fixed-width columns, binary data, and no headers. That's COBOL file handling.

### The Three File Organizations

COBOL supports three file organizations, each with its own use case:

1. **Sequential**: Records in order, like a tape
2. **Indexed (ISAM)**: Records with keys, like a simple database
3. **Relative**: Records by position number, like an array on disk

Think of them as:
- Sequential = LinkedList
- Indexed = HashMap
- Relative = ArrayList

### The ENVIRONMENT DIVISION Finally Matters

Remember that empty ENVIRONMENT DIVISION? For file handling, it's crucial:

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "CUSTMAST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               ALTERNATE RECORD KEY IS CUST-EMAIL WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRANSACTION-FILE
               ASSIGN TO "TRANS.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
```

Every file needs:
- **SELECT**: Internal name for the file
- **ASSIGN TO**: External file name/path
- **ORGANIZATION**: How records are stored
- **ACCESS MODE**: How you'll read/write
- **FILE STATUS**: Where to store operation results

### File Status: Your Error Detection System

```cobol
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS        PIC XX.
          88 SUCCESSFUL         VALUE "00".
          88 END-OF-FILE        VALUE "10".
          88 KEY-NOT-FOUND      VALUE "23".
          88 DUPLICATE-KEY      VALUE "22".
          88 FILE-NOT-FOUND     VALUE "35".
```

Every file operation updates the status. Always check it:

```cobol
       READ CUSTOMER-FILE
           INVALID KEY
               DISPLAY "Customer not found"
           NOT INVALID KEY
               PERFORM PROCESS-CUSTOMER
       END-READ.

       IF NOT SUCCESSFUL AND NOT END-OF-FILE
           DISPLAY "File error: " WS-FILE-STATUS
           PERFORM ERROR-ROUTINE
       END-IF.
```

### Sequential Files: The Simplest Case

Sequential files are like reading a book—start at the beginning, read to the end:

```cobol
       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05 EMP-ID           PIC 9(6).
           05 EMP-NAME         PIC X(30).
           05 EMP-SALARY       PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X VALUE 'N'.
           88 END-OF-FILE      VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           OPEN INPUT EMPLOYEE-FILE.

           PERFORM UNTIL END-OF-FILE
               READ EMPLOYEE-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PROCESS-EMPLOYEE
               END-READ
           END-PERFORM.

           CLOSE EMPLOYEE-FILE.

       PROCESS-EMPLOYEE.
           IF EMP-SALARY > 100000
               DISPLAY EMP-NAME " is highly compensated"
           END-IF.
```

**Pro tip**: Sequential files can only be read forward. No seeking backward, no random access. It's like a VHS tape—to replay something, you rewind the whole thing.

### Indexed Files: COBOL's Database

Indexed files (ISAM/VSAM) are COBOL's version of a database table:

```cobol
       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER
               ASSIGN TO "CUSTMAST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CM-CUSTOMER-ID
               ALTERNATE RECORD KEY IS CM-EMAIL
               ALTERNATE RECORD KEY IS CM-PHONE WITH DUPLICATES
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-MASTER.
       01  CUSTOMER-MASTER-REC.
           05 CM-CUSTOMER-ID    PIC 9(10).
           05 CM-EMAIL          PIC X(50).
           05 CM-PHONE          PIC X(10).
           05 CM-NAME           PIC X(30).
           05 CM-BALANCE        PIC S9(7)V99.
           05 CM-LAST-UPDATE    PIC 9(8).

       PROCEDURE DIVISION.
       UPDATE-CUSTOMER.
           MOVE 1234567890 TO CM-CUSTOMER-ID.

           * Random read by primary key
           READ CUSTOMER-MASTER
               INVALID KEY
                   DISPLAY "Customer not found"
                   PERFORM ADD-NEW-CUSTOMER
               NOT INVALID KEY
                   PERFORM UPDATE-EXISTING-CUSTOMER
           END-READ.

       ADD-NEW-CUSTOMER.
           INITIALIZE CUSTOMER-MASTER-REC.
           MOVE 1234567890 TO CM-CUSTOMER-ID.
           ACCEPT CM-NAME.
           ACCEPT CM-EMAIL.
           WRITE CUSTOMER-MASTER-REC
               INVALID KEY
                   DISPLAY "Duplicate customer ID!"
           END-WRITE.

       UPDATE-EXISTING-CUSTOMER.
           ADD WS-TRANSACTION-AMOUNT TO CM-BALANCE.
           MOVE FUNCTION CURRENT-DATE TO CM-LAST-UPDATE.
           REWRITE CUSTOMER-MASTER-REC
               INVALID KEY
                   DISPLAY "Update failed!"
           END-REWRITE.

       DELETE-CUSTOMER.
           DELETE CUSTOMER-MASTER
               INVALID KEY
                   DISPLAY "Delete failed!"
           END-DELETE.
```

### Access Modes: Choose Your Adventure

```cobol
* SEQUENTIAL ACCESS - Read in key order
ACCESS MODE IS SEQUENTIAL.
* Start at beginning or position
START CUSTOMER-FILE KEY IS > "5000000".
READ CUSTOMER-FILE NEXT.

* RANDOM ACCESS - Direct key lookup
ACCESS MODE IS RANDOM.
MOVE "12345" TO CM-CUSTOMER-ID.
READ CUSTOMER-FILE.

* DYNAMIC ACCESS - Both sequential and random
ACCESS MODE IS DYNAMIC.
* Random read
READ CUSTOMER-FILE.
* Then sequential
READ CUSTOMER-FILE NEXT.
```

### The START Statement: Positioning for Sequential Access

```cobol
       * Position at first record >= key value
       MOVE "SMITH" TO CM-LAST-NAME.
       START CUSTOMER-FILE KEY IS NOT < CM-LAST-NAME
           INVALID KEY
               DISPLAY "No customers found"
           NOT INVALID KEY
               PERFORM READ-ALL-SMITHS
       END-START.

       READ-ALL-SMITHS.
           PERFORM UNTIL WS-DONE = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-DONE
                   NOT AT END
                       IF CM-LAST-NAME NOT = "SMITH"
                           MOVE 'Y' TO WS-DONE
                       ELSE
                           DISPLAY CM-NAME
                       END-IF
               END-READ
           END-PERFORM.
```

### Relative Files: Arrays on Disk

```cobol
       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT SLOT-FILE
               ASSIGN TO "SLOTS.DAT"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS WS-SLOT-NUMBER.

       WORKING-STORAGE SECTION.
       01  WS-SLOT-NUMBER      PIC 9(4).

       PROCEDURE DIVISION.
       ACCESS-SLOT.
           MOVE 42 TO WS-SLOT-NUMBER.
           READ SLOT-FILE
               INVALID KEY
                   DISPLAY "Slot empty"
           END-READ.

           * Write to specific slot
           MOVE 100 TO WS-SLOT-NUMBER.
           WRITE SLOT-RECORD
               INVALID KEY
                   DISPLAY "Slot occupied"
           END-WRITE.
```

Relative files are like arrays—you access records by position number. Useful for fixed-size tables or reservation systems.

### File Locking and Sharing

COBOL handles concurrent access:

```cobol
       * Exclusive access
       OPEN I-O CUSTOMER-FILE WITH LOCK.

       * Shared access with record locking
       OPEN I-O CUSTOMER-FILE.
       READ CUSTOMER-FILE WITH LOCK.
       * Record is locked until REWRITE or UNLOCK

       * Manual unlock
       UNLOCK CUSTOMER-FILE.

       * No lock (read only)
       READ CUSTOMER-FILE WITH NO LOCK.
```

**Historical Note**: COBOL's file locking predates database transactions. It's primitive—no rollback, no ACID guarantees—but it works.

### Real-World File Processing Pattern

Here's a typical batch update program:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-UPDATE.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO "TRANS.DAT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT MASTER-FILE
               ASSIGN TO "MASTER.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MAST-KEY
               FILE STATUS IS WS-MASTER-STATUS.

           SELECT NEW-MASTER
               ASSIGN TO "MASTER.NEW"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS NEW-MAST-KEY.

           SELECT ERROR-FILE
               ASSIGN TO "ERRORS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANS-REC.
           05 TRANS-KEY        PIC 9(10).
           05 TRANS-TYPE       PIC X.
              88 ADD-TRANS     VALUE 'A'.
              88 UPDATE-TRANS  VALUE 'U'.
              88 DELETE-TRANS  VALUE 'D'.
           05 TRANS-AMOUNT     PIC S9(7)V99.

       FD  MASTER-FILE.
       01  MASTER-REC.
           05 MAST-KEY         PIC 9(10).
           05 MAST-BALANCE     PIC S9(7)V99.
           05 MAST-STATUS      PIC X.

       FD  NEW-MASTER.
       01  NEW-MASTER-REC.
           05 NEW-MAST-KEY     PIC 9(10).
           05 NEW-MAST-BALANCE PIC S9(7)V99.
           05 NEW-MAST-STATUS  PIC X.

       FD  ERROR-FILE.
       01  ERROR-REC          PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-MASTER-STATUS    PIC XX.
           88 REC-FOUND        VALUE "00".
           88 REC-NOT-FOUND    VALUE "23".

       01  WS-COUNTS.
           05 WS-READ-COUNT    PIC 9(9) VALUE ZERO.
           05 WS-ADD-COUNT     PIC 9(9) VALUE ZERO.
           05 WS-UPDATE-COUNT  PIC 9(9) VALUE ZERO.
           05 WS-DELETE-COUNT  PIC 9(9) VALUE ZERO.
           05 WS-ERROR-COUNT   PIC 9(9) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-CONTROL.
           PERFORM OPEN-FILES.
           PERFORM PROCESS-TRANSACTIONS.
           PERFORM CLOSE-FILES.
           PERFORM DISPLAY-STATISTICS.
           STOP RUN.

       OPEN-FILES.
           OPEN INPUT TRANSACTION-FILE.
           OPEN I-O MASTER-FILE.
           OPEN OUTPUT NEW-MASTER.
           OPEN OUTPUT ERROR-FILE.

       PROCESS-TRANSACTIONS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANSACTION-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-READ-COUNT
                       PERFORM PROCESS-ONE-TRANS
               END-READ
           END-PERFORM.

       PROCESS-ONE-TRANS.
           MOVE TRANS-KEY TO MAST-KEY.

           EVALUATE TRUE
               WHEN ADD-TRANS
                   PERFORM ADD-RECORD
               WHEN UPDATE-TRANS
                   PERFORM UPDATE-RECORD
               WHEN DELETE-TRANS
                   PERFORM DELETE-RECORD
               WHEN OTHER
                   PERFORM WRITE-ERROR
           END-EVALUATE.

       ADD-RECORD.
           READ MASTER-FILE.
           IF REC-FOUND
               STRING "Duplicate add for key " TRANS-KEY
                   INTO ERROR-REC
               WRITE ERROR-REC
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               INITIALIZE MASTER-REC
               MOVE TRANS-KEY TO MAST-KEY
               MOVE TRANS-AMOUNT TO MAST-BALANCE
               MOVE 'A' TO MAST-STATUS
               WRITE MASTER-REC
               ADD 1 TO WS-ADD-COUNT
           END-IF.

       UPDATE-RECORD.
           READ MASTER-FILE.
           IF REC-NOT-FOUND
               STRING "Update for missing key " TRANS-KEY
                   INTO ERROR-REC
               WRITE ERROR-REC
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               ADD TRANS-AMOUNT TO MAST-BALANCE
               REWRITE MASTER-REC
               ADD 1 TO WS-UPDATE-COUNT
           END-IF.

       DELETE-RECORD.
           READ MASTER-FILE.
           IF REC-NOT-FOUND
               STRING "Delete for missing key " TRANS-KEY
                   INTO ERROR-REC
               WRITE ERROR-REC
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               DELETE MASTER-FILE
               ADD 1 TO WS-DELETE-COUNT
           END-IF.
```

### Line Sequential Files: Text Files for Humans

```cobol
       * For reports and human-readable output
       SELECT REPORT-FILE
           ASSIGN TO "REPORT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

       FD  REPORT-FILE.
       01  REPORT-LINE         PIC X(132).

       * Writing formatted reports
       MOVE SPACES TO REPORT-LINE.
       STRING "Customer: " CUST-NAME
              " Balance: $" CUST-BALANCE
           INTO REPORT-LINE.
       WRITE REPORT-LINE.

       * Add blank line
       MOVE SPACES TO REPORT-LINE.
       WRITE REPORT-LINE.
```

### SORT and MERGE: Built-in File Operations

COBOL has built-in sorting:

```cobol
       * Sort file definition
       SD  SORT-FILE.
       01  SORT-REC.
           05 SORT-KEY         PIC 9(10).
           05 SORT-DATA        PIC X(70).

       * Sort operation
       SORT SORT-FILE
           ON ASCENDING KEY SORT-KEY
           USING INPUT-FILE
           GIVING OUTPUT-FILE.

       * Sort with input/output procedures
       SORT SORT-FILE
           ON DESCENDING KEY SORT-KEY
           INPUT PROCEDURE IS SELECT-RECORDS
           OUTPUT PROCEDURE IS CREATE-REPORT.

       SELECT-RECORDS.
           PERFORM UNTIL END-OF-INPUT
               READ INPUT-FILE
               IF CRITERIA-MET
                   RELEASE SORT-REC FROM INPUT-REC
               END-IF
           END-PERFORM.

       CREATE-REPORT.
           PERFORM UNTIL END-OF-SORT
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
               END-RETURN
               PERFORM WRITE-REPORT-LINE
           END-PERFORM.
```

### File Performance Tips

1. **Buffer properly**: Use BLOCK CONTAINS for better I/O
```cobol
FD  MASTER-FILE
    BLOCK CONTAINS 0 RECORDS.  * System optimized
```

2. **Choose right organization**: Sequential for batch, indexed for random

3. **Minimize I/O**: Read once, process in memory

4. **Use appropriate access mode**: Don't use DYNAMIC if you only need SEQUENTIAL

### Common File Handling Errors

```cobol
* Forgetting to check status
READ CUSTOMER-FILE.  * What if it fails?

* Not handling duplicates
WRITE CUSTOMER-REC.  * What if key exists?

* Leaving files open
* Always CLOSE your files!

* Wrong access mode
* Can't do random READ in SEQUENTIAL mode
```

### What You've Learned

COBOL's file handling is primitive by modern standards but incredibly reliable. No ORMs, no SQL injection, no schema migrations—just fixed records and predictable behavior. It's not elegant, but it's processed trillions of dollars worth of transactions.

---

*Next Chapter: [Chapter 6: Tables and Arrays](chapter-06-tables-and-arrays.md)*

*"In COBOL, every file is sacred, every record is great. If a record gets wasted, God gets quite irate."*