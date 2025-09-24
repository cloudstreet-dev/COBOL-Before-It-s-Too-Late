# Chapter 6: Tables and Arrays (But Not Like You Know Them)
## Where OCCURS Happens and Subscripts Start at 1

---

### Arrays? We Call Them Tables

In COBOL, arrays are called tables. Why? Because COBOL was designed by business people, and business people have tables of data, not arrays. This isn't just naming—it's a fundamental difference in thinking.

JavaScript array:
```javascript
let sales = [100, 200, 300];
sales.push(400);  // Dynamic!
console.log(sales[0]);  // 100 (zero-indexed)
```

COBOL table:
```cobol
01 WS-SALES-TABLE.
   05 WS-SALES OCCURS 12 TIMES PIC 9(7)V99.

MOVE 100.00 TO WS-SALES(1).  * ONE-indexed!
* Can't push. Size is fixed at 12. Forever.
```

Welcome to COBOL tables: fixed size, one-indexed, and absolutely no dynamic allocation. It's like arrays from a parallel universe where flexibility is illegal.

### The OCCURS Clause: Your Array Declaration

```cobol
* Simple one-dimensional table
01 WS-MONTH-NAMES.
   05 WS-MONTH OCCURS 12 TIMES PIC X(9).

* Initialize it (the hard way)
MOVE "January  " TO WS-MONTH(1).
MOVE "February " TO WS-MONTH(2).
MOVE "March    " TO WS-MONTH(3).
* ... and so on

* Or use VALUE clause with some creativity
01 WS-MONTH-TABLE.
   05 FILLER PIC X(108) VALUE
      "January  February March    April    May      June     July     August   SeptemberOctober  November December ".
01 WS-MONTH-NAMES REDEFINES WS-MONTH-TABLE.
   05 WS-MONTH OCCURS 12 TIMES PIC X(9).
```

That REDEFINES trick? That's peak COBOL cleverness—initialize a string, then view it as an array.

### Multi-Dimensional Tables: Because One Dimension Isn't Enough Suffering

```cobol
* 2D table: Sales by region and quarter
01 WS-SALES-MATRIX.
   05 WS-REGION OCCURS 5 TIMES.
      10 WS-QUARTERLY-SALES OCCURS 4 TIMES PIC 9(9)V99.

* Access: WS-QUARTERLY-SALES(3, 2) = Region 3, Quarter 2

* 3D table: Why not?
01 WS-INVENTORY.
   05 WS-WAREHOUSE OCCURS 10 TIMES.
      10 WS-PRODUCT OCCURS 100 TIMES.
         15 WS-MONTHLY-STOCK OCCURS 12 TIMES PIC 9(5).

* Access: WS-MONTHLY-STOCK(2, 50, 6) = Warehouse 2, Product 50, June
```

**Warning**: COBOL tables can go up to 7 dimensions. Just because you can doesn't mean you should. The programmer who maintains your 7-dimensional table will find you.

### Subscripts vs Indexes: The Performance Battle

COBOL has two ways to access table elements:

```cobol
* Using subscripts (simple but slower)
01 WS-SUB        PIC 99.

MOVE 5 TO WS-SUB.
DISPLAY WS-SALES(WS-SUB).

* Using indexes (faster, weirder)
01 WS-PRODUCT-TABLE.
   05 WS-PRODUCT OCCURS 100 TIMES
                 INDEXED BY PROD-IDX
                 PIC X(30).

SET PROD-IDX TO 1.
DISPLAY WS-PRODUCT(PROD-IDX).
SET PROD-IDX UP BY 1.  * Special index arithmetic
```

Indexes aren't numbers—they're memory offsets. You can't MOVE to them, you SET them. You can't ADD, you SET UP BY. It's like pointers with training wheels.

### The SEARCH Statement: Linear Search Built-In

```cobol
01 WS-STATE-TABLE.
   05 WS-STATE-ENTRY OCCURS 50 TIMES
                     INDEXED BY STATE-IDX.
      10 WS-STATE-CODE    PIC XX.
      10 WS-STATE-NAME    PIC X(20).

* Sequential search
SET STATE-IDX TO 1.
SEARCH WS-STATE-ENTRY
    AT END
        DISPLAY "State not found"
    WHEN WS-STATE-CODE(STATE-IDX) = "CA"
        DISPLAY "Found: " WS-STATE-NAME(STATE-IDX)
END-SEARCH.
```

SEARCH does a linear search from the current index position. It's O(n), but it's built into the language.

### SEARCH ALL: Binary Search (With Conditions)

```cobol
* Table MUST be sorted for SEARCH ALL
01 WS-CUSTOMER-TABLE.
   05 WS-CUSTOMER OCCURS 1000 TIMES
                  ASCENDING KEY IS WS-CUST-ID
                  INDEXED BY CUST-IDX.
      10 WS-CUST-ID       PIC 9(10).
      10 WS-CUST-NAME     PIC X(30).
      10 WS-CUST-BALANCE  PIC S9(7)V99.

* Binary search - O(log n)
SEARCH ALL WS-CUSTOMER
    AT END
        DISPLAY "Customer not found"
    WHEN WS-CUST-ID(CUST-IDX) = 1234567890
        DISPLAY "Found: " WS-CUST-NAME(CUST-IDX)
END-SEARCH.
```

SEARCH ALL does binary search, but only on sorted tables with ASCENDING/DESCENDING KEY clause. It's fast, but rigid.

### Variable-Length Tables: COBOL's Dynamic Arrays (Sort Of)

```cobol
01 WS-DYNAMIC-TABLE.
   05 WS-ITEM-COUNT      PIC 999 VALUE ZERO.
   05 WS-ITEM OCCURS 1 TO 999 TIMES
              DEPENDING ON WS-ITEM-COUNT
              PIC X(50).

* Add items (manually manage the count)
ADD 1 TO WS-ITEM-COUNT.
MOVE "New Item" TO WS-ITEM(WS-ITEM-COUNT).

* Process only actual items
PERFORM VARYING WS-I FROM 1 BY 1
        UNTIL WS-I > WS-ITEM-COUNT
    DISPLAY WS-ITEM(WS-I)
END-PERFORM.
```

This isn't truly dynamic—memory for 999 items is always allocated. The DEPENDING ON just tells COBOL how many to process.

### Table Initialization: The Painful Truth

```cobol
* Initialize all elements to spaces/zeros
01 WS-TABLE.
   05 WS-ENTRY OCCURS 100 TIMES.
      10 WS-CODE         PIC XXX.
      10 WS-AMOUNT       PIC 9(5)V99.

INITIALIZE WS-TABLE.  * Everything becomes spaces/zeros

* Initialize to specific values (the hard way)
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
    MOVE "ABC" TO WS-CODE(WS-I)
    MOVE 100.00 TO WS-AMOUNT(WS-I)
END-PERFORM.

* Using VALUE for small tables
01 WS-DAYS-IN-MONTH.
   05 FILLER PIC X(24) VALUE "312831303130313130313031".
01 WS-DAYS-TABLE REDEFINES WS-DAYS-IN-MONTH.
   05 WS-DAYS OCCURS 12 TIMES PIC 99.
* Now WS-DAYS(2) = 28 (February)
```

### Real-World Example: Rate Table Lookup

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Tax rate table by income bracket
       01 WS-TAX-TABLE.
          05 WS-TAX-BRACKET OCCURS 7 TIMES
                            INDEXED BY TAX-IDX.
             10 WS-INCOME-LIMIT    PIC 9(7).
             10 WS-TAX-RATE        PIC V999.

       01 WS-TABLE-INITIALIZED    PIC X VALUE 'N'.
          88 TABLE-READY          VALUE 'Y'.

       01 WS-INCOME               PIC 9(7)V99.
       01 WS-TAX-DUE              PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZE-TAX-TABLE.
           ACCEPT WS-INCOME.
           PERFORM CALCULATE-TAX.
           DISPLAY "Tax due: $" WS-TAX-DUE.

       INITIALIZE-TAX-TABLE.
           MOVE 10000 TO WS-INCOME-LIMIT(1).
           MOVE .100 TO WS-TAX-RATE(1).

           MOVE 40000 TO WS-INCOME-LIMIT(2).
           MOVE .120 TO WS-TAX-RATE(2).

           MOVE 85000 TO WS-INCOME-LIMIT(3).
           MOVE .220 TO WS-TAX-RATE(3).

           MOVE 163000 TO WS-INCOME-LIMIT(4).
           MOVE .240 TO WS-TAX-RATE(4).

           MOVE 207000 TO WS-INCOME-LIMIT(5).
           MOVE .320 TO WS-TAX-RATE(5).

           MOVE 518000 TO WS-INCOME-LIMIT(6).
           MOVE .350 TO WS-TAX-RATE(6).

           MOVE 9999999 TO WS-INCOME-LIMIT(7).
           MOVE .370 TO WS-TAX-RATE(7).

           SET TABLE-READY TO TRUE.

       CALCULATE-TAX.
           SET TAX-IDX TO 1.
           SEARCH WS-TAX-BRACKET
               WHEN WS-INCOME <= WS-INCOME-LIMIT(TAX-IDX)
                   COMPUTE WS-TAX-DUE =
                       WS-INCOME * WS-TAX-RATE(TAX-IDX)
           END-SEARCH.
```

### Table Passing: Arrays as Parameters

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER-TABLE.
          05 WS-NUMBER OCCURS 10 TIMES PIC 9(5).

       PROCEDURE DIVISION.
           * Fill table
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               COMPUTE WS-NUMBER(WS-I) = WS-I * 100
           END-PERFORM.

           * Pass entire table to subprogram
           CALL 'PROCESS-TABLE' USING WS-NUMBER-TABLE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-TABLE.

       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NUMBER-TABLE.
          05 LS-NUMBER OCCURS 10 TIMES PIC 9(5).

       PROCEDURE DIVISION USING LS-NUMBER-TABLE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               DISPLAY "Number " WS-I ": " LS-NUMBER(WS-I)
           END-PERFORM.
```

### Common Table Patterns

#### Pattern 1: Lookup Table
```cobol
01 WS-PRODUCT-LOOKUP.
   05 WS-PRODUCT-ENTRY OCCURS 1000 TIMES
                       INDEXED BY PROD-IDX.
      10 WS-PROD-CODE     PIC X(10).
      10 WS-PROD-NAME     PIC X(30).
      10 WS-PROD-PRICE    PIC 9(5)V99.

* Load from file at startup
PERFORM VARYING PROD-IDX FROM 1 BY 1
        UNTIL END-OF-FILE OR PROD-IDX > 1000
    READ PRODUCT-FILE
    MOVE PROD-CODE TO WS-PROD-CODE(PROD-IDX)
    MOVE PROD-NAME TO WS-PROD-NAME(PROD-IDX)
    MOVE PROD-PRICE TO WS-PROD-PRICE(PROD-IDX)
END-PERFORM.
```

#### Pattern 2: Accumulator Arrays
```cobol
01 WS-MONTHLY-TOTALS.
   05 WS-MONTH-TOTAL OCCURS 12 TIMES PIC 9(9)V99 VALUE ZERO.

* Accumulate throughout processing
ADD SALE-AMOUNT TO WS-MONTH-TOTAL(SALE-MONTH).
```

#### Pattern 3: Parallel Arrays (Don't Do This)
```cobol
* Bad: Parallel arrays
01 WS-CUSTOMER-IDS.
   05 WS-CUST-ID OCCURS 100 TIMES PIC 9(10).
01 WS-CUSTOMER-NAMES.
   05 WS-CUST-NAME OCCURS 100 TIMES PIC X(30).

* Good: Structure array
01 WS-CUSTOMERS.
   05 WS-CUSTOMER OCCURS 100 TIMES.
      10 WS-CUST-ID    PIC 9(10).
      10 WS-CUST-NAME  PIC X(30).
```

### Performance Considerations

1. **Indexes are faster than subscripts** - Use INDEXED BY when possible

2. **SEARCH ALL beats manual search** - But requires sorted data

3. **Memory alignment matters** - Align table elements to word boundaries

4. **Consider table size carefully** - COBOL allocates all memory upfront

### Table Gotchas

```cobol
* Gotcha 1: One-based indexing
MOVE WS-DATA(0) TO WS-OUTPUT.  * Runtime error!

* Gotcha 2: Fixed size
05 WS-ITEM OCCURS 10 TIMES.
MOVE "Data" TO WS-ITEM(11).  * Runtime error!

* Gotcha 3: No bounds checking in some compilers
* This might corrupt memory:
MOVE WS-DATA(999999) TO WS-OUTPUT.

* Gotcha 4: SEARCH needs index positioning
SET STATE-IDX TO 1.  * Must set before SEARCH
SEARCH WS-STATE-ENTRY...

* Gotcha 5: SEARCH ALL needs sorted data
* If not sorted, results are undefined
```

### Modern Table Features

Recent COBOL standards added:

```cobol
* Table sorting (COBOL 2002)
SORT WS-CUSTOMER ASCENDING KEY WS-CUST-NAME.

* Reference modification on tables
DISPLAY WS-PRODUCT(5)(1:10).  * First 10 chars of 5th element

* ALLOCATE/FREE for dynamic tables (COBOL 2002)
ALLOCATE WS-DYNAMIC-TABLE.
FREE WS-DYNAMIC-TABLE.
```

But most production code predates these luxuries.

### Tables vs Modern Collections

| COBOL Tables | Modern Arrays/Lists |
|-------------|-------------------|
| Fixed size | Dynamic size |
| One-indexed | Zero-indexed |
| No methods | Rich API (.map, .filter, etc.) |
| Manual search | Built-in operations |
| Type-safe | Often type-safe |
| Memory efficient | Memory flexible |

### Practical Example: Order Processing with Tables

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Order with line items
       01 WS-ORDER.
          05 WS-ORDER-ID          PIC 9(10).
          05 WS-LINE-COUNT        PIC 99 VALUE ZERO.
          05 WS-ORDER-LINE OCCURS 50 TIMES
                           INDEXED BY LINE-IDX.
             10 WS-PRODUCT-CODE   PIC X(10).
             10 WS-QUANTITY       PIC 999.
             10 WS-UNIT-PRICE     PIC 9(5)V99.
             10 WS-LINE-TOTAL     PIC 9(7)V99.

       01 WS-ORDER-TOTAL          PIC 9(9)V99.

       PROCEDURE DIVISION.
       PROCESS-ORDER.
           PERFORM ADD-LINE-ITEM.
           PERFORM CALCULATE-ORDER-TOTAL.
           PERFORM DISPLAY-ORDER.

       ADD-LINE-ITEM.
           ADD 1 TO WS-LINE-COUNT.
           SET LINE-IDX TO WS-LINE-COUNT.

           ACCEPT WS-PRODUCT-CODE(LINE-IDX).
           ACCEPT WS-QUANTITY(LINE-IDX).
           ACCEPT WS-UNIT-PRICE(LINE-IDX).

           COMPUTE WS-LINE-TOTAL(LINE-IDX) =
                   WS-QUANTITY(LINE-IDX) *
                   WS-UNIT-PRICE(LINE-IDX).

       CALCULATE-ORDER-TOTAL.
           MOVE ZERO TO WS-ORDER-TOTAL.
           PERFORM VARYING LINE-IDX FROM 1 BY 1
                   UNTIL LINE-IDX > WS-LINE-COUNT
               ADD WS-LINE-TOTAL(LINE-IDX) TO WS-ORDER-TOTAL
           END-PERFORM.

       DISPLAY-ORDER.
           DISPLAY "Order ID: " WS-ORDER-ID.
           PERFORM VARYING LINE-IDX FROM 1 BY 1
                   UNTIL LINE-IDX > WS-LINE-COUNT
               DISPLAY "Item " LINE-IDX ": "
                       WS-PRODUCT-CODE(LINE-IDX)
                       " Qty: " WS-QUANTITY(LINE-IDX)
                       " Total: $" WS-LINE-TOTAL(LINE-IDX)
           END-PERFORM.
           DISPLAY "Order Total: $" WS-ORDER-TOTAL.
```

### What You've Learned

COBOL tables are arrays trapped in 1959. They're fixed-size, one-indexed, and require manual management for everything. But they're also predictable, memory-efficient, and have processed decades of business data without the luxury of ArrayList or Vector.

The key to COBOL tables is understanding their limitations and working within them. No dynamic allocation? Size your tables appropriately. No built-in methods? Write your own routines. One-indexed? Get used to it (or get bugs).

---

*Next Chapter: [Chapter 7: String Manipulation and Report Generation](chapter-07-strings-and-reports.md)*

*"In COBOL, arrays start at 1 because 0 hadn't been invented yet."*