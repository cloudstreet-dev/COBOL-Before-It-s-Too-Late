# Chapter 2: COBOL Basics for Modern Programmers
## Your First Day in 1959 (With Better Coffee)

---

### Welcome to the Structure Wars

If modern programming languages are jazz—improvisational, flexible, surprising—then COBOL is a military march. Every step is defined. Every movement is regulated. And God help you if you put something in column 7.

But here's the secret: Once you understand COBOL's rigid structure, it becomes oddly comforting. It's like writing sonnets. The constraints force clarity.

### The Four Divisions: COBOL's Sacred Geometry

Every COBOL program has four divisions. Always four. Always in this order:

1. **IDENTIFICATION DIVISION** - "Hello, my name is..."
2. **ENVIRONMENT DIVISION** - "I run on..."
3. **DATA DIVISION** - "I work with..."
4. **PROCEDURE DIVISION** - "Here's what I actually do"

Think of it like a formal letter:
- Return address (IDENTIFICATION)
- Envelope (ENVIRONMENT)
- Attachments (DATA)
- The actual letter (PROCEDURE)

Let's write a real program to see this in action:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMP-CONVERTER.
       AUTHOR. CLAUDE-CODE-OPUS-4-1.
       DATE-WRITTEN. 2025-01-01.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CELSIUS      PIC S9(3)V9(2).
       01 WS-FAHRENHEIT   PIC S9(3)V9(2).
       01 WS-CHOICE       PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Temperature Converter".
           DISPLAY "C for Celsius to Fahrenheit".
           DISPLAY "F for Fahrenheit to Celsius".
           DISPLAY "Enter choice: ".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = "C" OR WS-CHOICE = "c"
               PERFORM CELSIUS-TO-FAHRENHEIT
           ELSE
               IF WS-CHOICE = "F" OR WS-CHOICE = "f"
                   PERFORM FAHRENHEIT-TO-CELSIUS
               ELSE
                   DISPLAY "Invalid choice"
               END-IF
           END-IF.

           STOP RUN.

       CELSIUS-TO-FAHRENHEIT.
           DISPLAY "Enter Celsius: ".
           ACCEPT WS-CELSIUS.
           COMPUTE WS-FAHRENHEIT = (WS-CELSIUS * 9 / 5) + 32.
           DISPLAY WS-CELSIUS "C = " WS-FAHRENHEIT "F".

       FAHRENHEIT-TO-CELSIUS.
           DISPLAY "Enter Fahrenheit: ".
           ACCEPT WS-FAHRENHEIT.
           COMPUTE WS-CELSIUS = (WS-FAHRENHEIT - 32) * 5 / 9.
           DISPLAY WS-FAHRENHEIT "F = " WS-CELSIUS "C".
```

### Breaking Down the Madness

#### IDENTIFICATION DIVISION: The Business Card

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMP-CONVERTER.
```

This is mandatory. Every program needs an ID. In JavaScript, you might casually write `// Temperature converter`. In COBOL, you formally declare your program's identity like it's applying for a passport.

**Historical Note**: The AUTHOR and DATE-WRITTEN entries used to be mandatory. Companies would track who to blame when something broke. Now they're optional, but you'll still see them in legacy code, like finding "AUTHOR. JOHNSON-FROM-ACCOUNTING." who retired in 1987.

#### ENVIRONMENT DIVISION: The Dating Profile

```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
```

This specifies what computer you're writing for and what computer will run it. In 2025, this is mostly ceremonial, like saying "Works on my machine" but formally.

Most modern COBOL programs leave this empty or minimal. But in the mainframe world, this matters. Different IBM machines had different capabilities, and COBOL needed to know.

#### DATA DIVISION: Where Variables Go to Get Dressed Up

This is where COBOL gets weird for modern programmers. You don't declare variables inline. You declare them ALL upfront, with their exact size and type.

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CELSIUS      PIC S9(3)V9(2).
```

Let's decode this insanity:
- `01` - Level number (think: top-level variable)
- `WS-CELSIUS` - Variable name (WS = Working Storage, a naming convention)
- `PIC` - Picture clause (yes, really)
- `S9(3)V9(2)` - Signed number, 3 digits before decimal, 2 after

In JavaScript, you'd write:
```javascript
let celsius; // Could be anything! String? Number? Object? Who knows!
```

In COBOL:
```cobol
01 WS-CELSIUS PIC S9(3)V9(2). * Exactly 3 digits, decimal, 2 digits, signed, no surprises
```

### The Picture Clause: COBOL's Type System

The PIC (PICTURE) clause is how COBOL defines data types. It's like regex met a type system and had a very strict baby:

| PIC Clause | Meaning | JavaScript Equivalent |
|------------|---------|----------------------|
| `PIC X(10)` | 10 characters | `string` (max 10 chars) |
| `PIC 9(5)` | 5 digits | `number` (max 99999) |
| `PIC S9(3)` | Signed 3 digits | `number` (-999 to 999) |
| `PIC 9(3)V99` | 3 digits, decimal, 2 digits | `number` (##.##) |
| `PIC $$$,$$9.99` | Currency with formatting | `string` (formatted) |

Yes, that last one is real. COBOL will literally format your currency for you:

```cobol
01 WS-SALARY PIC $$$,$$9.99 VALUE 50000.00.
DISPLAY WS-SALARY.  * Shows: $50,000.00
```

Try getting JavaScript to do that without a library. I'll wait.

### The Column Prison (Or: Why Everything Looks Weird)

Traditional COBOL is column-sensitive, a hangover from punch card days:

```
Columns 1-6:   Sequence number (ignored now, used for card sorting)
Column 7:      Special indicators (* for comments, - for continuation)
Columns 8-11:  Division/Section/Paragraph headers (Area A)
Columns 12-72: Your actual code (Area B)
Columns 73-80: Program identification (ignored now)
```

So when you see:
```cobol
      * This is a comment
       IDENTIFICATION DIVISION.
           DISPLAY "Hello World".
```

That spacing isn't artistic choice. It's the law.

**Modern Reality Check**: Most modern COBOL compilers have a "free format" mode where you can ignore all this. But you WILL encounter old code following these rules, and some shops still enforce them because "that's how we've always done it."

### Your First Real Business Logic

Let's write something you might actually encounter: a customer discount calculator.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISCOUNT-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-RECORD.
          05 WS-CUSTOMER-ID      PIC 9(6).
          05 WS-CUSTOMER-NAME    PIC X(30).
          05 WS-PURCHASE-AMOUNT  PIC 9(5)V99.
          05 WS-CUSTOMER-TYPE    PIC X.
             88 GOLD-MEMBER      VALUE 'G'.
             88 SILVER-MEMBER    VALUE 'S'.
             88 REGULAR-MEMBER   VALUE 'R'.

       01 WS-DISCOUNT-AMOUNT     PIC 9(5)V99.
       01 WS-FINAL-AMOUNT        PIC 9(5)V99.
       01 WS-DISPLAY-AMOUNT      PIC $$$,$$9.99.

       PROCEDURE DIVISION.
       CALCULATE-DISCOUNT.
           DISPLAY "Enter Customer ID: ".
           ACCEPT WS-CUSTOMER-ID.
           DISPLAY "Enter Customer Name: ".
           ACCEPT WS-CUSTOMER-NAME.
           DISPLAY "Enter Purchase Amount: ".
           ACCEPT WS-PURCHASE-AMOUNT.
           DISPLAY "Enter Customer Type (G/S/R): ".
           ACCEPT WS-CUSTOMER-TYPE.

           EVALUATE TRUE
               WHEN GOLD-MEMBER
                   COMPUTE WS-DISCOUNT-AMOUNT =
                           WS-PURCHASE-AMOUNT * 0.20
               WHEN SILVER-MEMBER
                   COMPUTE WS-DISCOUNT-AMOUNT =
                           WS-PURCHASE-AMOUNT * 0.10
               WHEN REGULAR-MEMBER
                   COMPUTE WS-DISCOUNT-AMOUNT =
                           WS-PURCHASE-AMOUNT * 0.05
               WHEN OTHER
                   MOVE ZERO TO WS-DISCOUNT-AMOUNT
           END-EVALUATE.

           SUBTRACT WS-DISCOUNT-AMOUNT FROM WS-PURCHASE-AMOUNT
               GIVING WS-FINAL-AMOUNT.

           MOVE WS-FINAL-AMOUNT TO WS-DISPLAY-AMOUNT.

           DISPLAY "Customer: " WS-CUSTOMER-NAME.
           DISPLAY "Original Amount: $" WS-PURCHASE-AMOUNT.
           DISPLAY "Discount: $" WS-DISCOUNT-AMOUNT.
           DISPLAY "Final Amount: " WS-DISPLAY-AMOUNT.

           STOP RUN.
```

### The Weird Parts That Make Sense

#### Level Numbers: COBOL's Object Structure

See those `01` and `05` numbers? Those are level numbers, and they create hierarchies:

```cobol
01 WS-CUSTOMER-RECORD.
   05 WS-CUSTOMER-ID      PIC 9(6).
   05 WS-CUSTOMER-NAME    PIC X(30).
```

This is like JavaScript:
```javascript
const customerRecord = {
    customerId: 0,
    customerName: ""
};
```

But in COBOL, you can reference the whole structure (`WS-CUSTOMER-RECORD`) or individual fields (`WS-CUSTOMER-ID`).

#### 88 Levels: COBOL's Enums

```cobol
05 WS-CUSTOMER-TYPE    PIC X.
   88 GOLD-MEMBER      VALUE 'G'.
   88 SILVER-MEMBER    VALUE 'S'.
```

These are condition names. Instead of writing:
```cobol
IF WS-CUSTOMER-TYPE = 'G'
```

You write:
```cobol
IF GOLD-MEMBER
```

It's actually quite elegant, once you get over the number 88.

### COBOL's Weird Arithmetic

COBOL has multiple ways to do math, because why not:

```cobol
* Method 1: COMPUTE (like normal languages)
COMPUTE WS-TOTAL = WS-PRICE * WS-QUANTITY.

* Method 2: ADD (verbose but clear)
ADD WS-PRICE TO WS-TOTAL.

* Method 3: ADD with GIVING (non-destructive)
ADD WS-PRICE WS-TAX GIVING WS-TOTAL.

* Method 4: MULTIPLY (because why not)
MULTIPLY WS-PRICE BY WS-QUANTITY GIVING WS-TOTAL.

* Method 5: The truly insane
ADD 1 TO WS-COUNTER
    ON SIZE ERROR
        DISPLAY "Counter overflow!"
    NOT ON SIZE ERROR
        DISPLAY "Counter incremented"
END-ADD.
```

Yes, COBOL has overflow detection built into basic arithmetic. Your JavaScript `Number.MAX_SAFE_INTEGER` problems don't exist here.

### The MOVE Statement: Not What You Think

In COBOL, MOVE doesn't move anything. It copies:

```cobol
MOVE WS-CUSTOMER-NAME TO WS-DISPLAY-NAME.
* WS-CUSTOMER-NAME still has its value!
```

But MOVE has superpowers:

```cobol
* Initialize multiple fields
MOVE ZEROS TO WS-COUNTER WS-TOTAL WS-DISCOUNT.

* Initialize a whole structure
MOVE SPACES TO WS-CUSTOMER-RECORD.

* Type conversion with padding
MOVE 123 TO WS-DISPLAY-NUMBER.  * "00123" if PIC 9(5)
```

### Strings: The Pain Point

String manipulation in COBOL is... special. There's no `+` operator for concatenation:

```cobol
* JavaScript: name = firstName + " " + lastName;

* COBOL Method 1: STRING
STRING WS-FIRST-NAME DELIMITED BY SPACE
       " " DELIMITED BY SIZE
       WS-LAST-NAME DELIMITED BY SPACE
       INTO WS-FULL-NAME
END-STRING.

* COBOL Method 2: Just put them next to each other
DISPLAY WS-FIRST-NAME " " WS-LAST-NAME.
```

We'll cover this nightmare in detail in Chapter 7. For now, just know it exists.

### Your First COBOL Gotchas

1. **Everything is UPPERCASE** (traditionally)
   - Modern compilers don't care, but tradition is strong

2. **Periods are statement terminators**
   - Miss one, and your program won't compile
   - Have an extra one, and half your logic gets skipped

3. **No implicit type conversion**
   - COBOL won't let you add a string to a number
   - It won't even try

4. **Fixed-size everything**
   - Declare a 10-character name field?
   - "Bob" becomes "Bob       " (7 spaces)

5. **No null/undefined**
   - Variables always have a value
   - Usually spaces (for text) or zeros (for numbers)

### Modern COBOL: It's Not 1959 Anymore

Modern COBOL (yes, that's a thing) has evolved:

```cobol
* Object-oriented COBOL (yes, really)
CLASS-ID. Customer INHERITS FROM Person.

* Inline variable declaration (finally!)
COMPUTE discount = purchase-amount * 0.10.

* Free format (no more columns!)
PROGRAM-ID. ModernProgram.
IF customer-type = 'GOLD'
    COMPUTE discount = amount * 0.20
ELSE
    COMPUTE discount = amount * 0.10
END-IF.
```

But you'll rarely see this in the wild. The COBOL that needs maintaining was written when bell-bottoms were fashionable (the first time).

### Practical Exercise: FizzBuzz in COBOL

Every programmer knows FizzBuzz. Here's the COBOL version:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER     PIC 9(3).
       01 WS-REMAINDER   PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
                   UNTIL WS-COUNTER > 100

               DIVIDE WS-COUNTER BY 15
                   GIVING WS-REMAINDER REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   DISPLAY "FizzBuzz"
               ELSE
                   DIVIDE WS-COUNTER BY 3
                       GIVING WS-REMAINDER REMAINDER WS-REMAINDER
                   IF WS-REMAINDER = 0
                       DISPLAY "Fizz"
                   ELSE
                       DIVIDE WS-COUNTER BY 5
                           GIVING WS-REMAINDER REMAINDER WS-REMAINDER
                       IF WS-REMAINDER = 0
                           DISPLAY "Buzz"
                       ELSE
                           DISPLAY WS-COUNTER
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           STOP RUN.
```

Compare this to JavaScript:
```javascript
for(let i = 1; i <= 100; i++) {
    if(i % 15 === 0) console.log("FizzBuzz");
    else if(i % 3 === 0) console.log("Fizz");
    else if(i % 5 === 0) console.log("Buzz");
    else console.log(i);
}
```

The COBOL version is longer, more explicit, and leaves no room for ambiguity. That's COBOL in a nutshell.

### What You've Learned (The Trauma Summary)

1. **Four Divisions**: The unchangeable structure of every COBOL program
2. **PIC Clauses**: How COBOL thinks about data types (very specifically)
3. **Level Numbers**: COBOL's way of creating data structures
4. **Verbosity**: Why COBOL programs read like legal documents
5. **Column Sensitivity**: The punch card prison we're still in

### What's Next

In Chapter 3, we're diving deep into the DATA DIVISION. You'll learn:
- How COBOL handles complex data structures
- The difference between WORKING-STORAGE and LOCAL-STORAGE
- REDEFINES (COBOL's union types, sort of)
- Why COBOL's decimal arithmetic makes accountants happy
- Data validation that would make TypeScript jealous

But first, take a break. Go write some Python or JavaScript. Appreciate your `let` statements and your dynamic typing. Because from here on, we're going deep into the world where every byte counts and every column matters.

Remember: COBOL isn't bad. It's just different. Like your grandfather who insists on writing checks at the grocery store—it works, it's reliable, and it's not going anywhere.

---

*Next Chapter: [Chapter 3: Data Division Deep Dive](chapter-03-data-division.md)*

*"In COBOL, we don't have bugs. We have 'unexpected features that have been in production for 30 years.'"*