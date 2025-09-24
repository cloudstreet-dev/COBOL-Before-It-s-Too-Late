# Chapter 1: Why COBOL Still Matters
## Or: How I Learned to Stop Worrying and Love the DIVISION

---

### The Elephant in the Server Room

Let me paint you a picture. It's 3 AM, and somewhere in a nondescript data center, a COBOL program is quietly processing your credit card transaction. This program was written in 1982. The original programmer retired in 2005. Their replacement retired in 2018. The documentation is a three-ring binder that no one can find. Yet this program has run, without fail, every single night for four decades.

This isn't a horror story. It's Tuesday.

### The Numbers Don't Lie (Unlike Some Modern Apps)

Here's what you need to understand about COBOL's footprint in 2025:

- **95% of ATM transactions** touch COBOL code
- **80% of in-person credit card transactions** involve COBOL
- **70% of Fortune 500 companies** still run COBOL systems
- **$3 trillion** in commerce flows through COBOL programs daily
- **1.5 billion** new lines of COBOL are written annually

Yes, you read that last one correctly. New COBOL code. Being written. Right now.

### A Brief History of Not Dying

**1959**: Grace Hopper and her team create COBOL (COmmon Business-Oriented Language). The goal? A programming language that business people could read. The result? A language so verbose that even programmers need coffee to get through it.

```cobol
ADD 1 TO CUSTOMER-COUNT GIVING NEW-CUSTOMER-COUNT.
```

Compare this to JavaScript:
```javascript
newCustomerCount = customerCount + 1;
```

**Historical Note**: Grace Hopper believed code should be readable by managers and auditors. She succeeded. Whether this was a good thing is still debated in therapy sessions worldwide.

**1960s-1970s**: COBOL becomes the language of choice for business applications. Why? Because it works. It's boring, predictable, and reliable—exactly what you want handling your payroll.

**1980s**: "COBOL is dead!" cry the C programmers. COBOL shrugs and processes another trillion dollars.

**1990s**: "COBOL is dead!" shout the Java developers. COBOL yawns and handles Y2K (more on that later).

**2000s**: "COBOL is dead!" proclaim the web developers. COBOL chuckles and keeps running.

**2010s**: "COBOL is dead!" insist the Node.js folks. COBOL doesn't even look up from processing your tax return.

**2020**: COVID-19 hits. Unemployment systems crash. States desperately search for COBOL programmers. COBOL smirks.

**2025**: Here we are. COBOL is still "dying." It's been "dying" for 65 years.

### The Y2K Vindication

Remember Y2K? When we thought civilization would collapse because programmers in the '60s used two-digit years? That was COBOL's moment to shine. Not because it caused the problem (though it did), but because it proved something crucial: **COBOL systems are too important to replace**.

Companies spent billions updating COBOL code rather than replacing it. Why? Because replacing a COBOL system is like replacing the foundation of a skyscraper while people are still working on the 50th floor.

**Fun Fact**: The Y2K remediation effort created more demand for COBOL programmers than the previous decade combined. Some contractors made $500+ per hour. Not bad for a "dead" language.

### Why COBOL Refuses to Die

#### 1. It's Extremely Good at What It Does

COBOL excels at:
- Decimal arithmetic (no floating-point errors on your money!)
- Batch processing (millions of records? No problem)
- Transaction processing (ACID compliance before it was cool)
- Report generation (those bank statements aren't writing themselves)

Try calculating compound interest in JavaScript with floating-point numbers. Now try it with billions of dollars. See the problem?

#### 2. The Cost of Replacement is Astronomical

Let's say you have a COBOL system with 10 million lines of code (not uncommon). To replace it, you need to:
1. Understand what all 10 million lines do (good luck with that)
2. Replicate the business logic exactly (including the bugs that became features)
3. Migrate decades of data without losing a penny
4. Test everything (hope you have 5 years)
5. Train everyone on the new system
6. Deal with the lawsuits when something goes wrong

Estimated cost? $100 million and up. Estimated risk? Your career.

#### 3. If It Ain't Broke...

COBOL systems are the diesel engines of the programming world. They're not sexy, they're not fast, but they run forever with minimal maintenance. That program from 1982 I mentioned? It's processed over $500 billion in transactions without a single data corruption error.

Your microservices architecture can't even stay up through a Black Friday sale.

### The COBOL Programmer Shortage Crisis

Here's the problem: The average COBOL programmer is 58 years old. In the next 5-10 years, most will retire. Meanwhile, computer science programs haven't taught COBOL since the Clinton administration.

This creates an interesting opportunity. While your friends are competing with 10,000 other JavaScript developers for the same job, COBOL programmers are getting signing bonuses and picking their projects.

**Real conversation from 2024:**
- Recruiter: "Do you know COBOL?"
- Programmer: "I've... heard of it?"
- Recruiter: "Great! When can you start? Name your price."

### What This Means for You

If you're reading this book, you're probably in one of three situations:

1. **The Volunteer**: Your company needs someone to maintain COBOL systems, and you drew the short straw (or saw the salary).

2. **The Mercenary**: You heard COBOL programmers make bank and want in on the action.

3. **The Curious**: You want to understand the technology that secretly runs the world.

Whatever brought you here, know this: Learning COBOL in 2025 isn't learning a dead language. It's learning a language that will outlive us all.

### The JavaScript Developer's Guide to COBOL Thinking

Since many of you come from modern language backgrounds, let's translate some concepts:

| Modern Concept | COBOL Equivalent | Why It's Different |
|---------------|------------------|-------------------|
| `let x = 5` | `01 X PIC 9 VALUE 5.` | COBOL declares the exact type and size |
| Objects | Records | Everything is a struct, basically |
| Functions | Paragraphs/Sections | Code organization from the typewriter era |
| `if (x) {...}` | `IF X IS EQUAL TO...` | COBOL spells EVERYTHING out |
| npm packages | COPY books | Reusable code, 1970s style |
| async/await | N/A | COBOL is beautifully, brutally synchronous |

### The Cultural Shock

Coming from modern programming to COBOL is like time travel. You'll experience:

**The Verbosity**: COBOL doesn't believe in shortcuts. Where Python has `len()`, COBOL has `FUNCTION LENGTH OF`. Where Java has `++`, COBOL has `ADD 1 TO`.

**The Structure**: Four divisions, always in the same order. It's like writing a formal letter every time you code.

**The Column Sensitivity**: Traditional COBOL cares about columns 1-6, 7, 8-11, 12-72, and 73-80. Yes, this is from punch cards. No, we can't change it.

**The Business Focus**: COBOL doesn't have fancy data structures or clever algorithms. It has MOVE statements and arithmetic. Because that's what business needs.

### Your First Taste of COBOL

Here's a simple COBOL program that would make any JavaScript developer weep:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(30) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "What is your name? ".
           ACCEPT WS-NAME.
           DISPLAY "Hello, " WS-NAME.
           STOP RUN.
```

That's 13 lines to do what JavaScript does in 2:
```javascript
const name = prompt("What is your name?");
console.log(`Hello, ${name}`);
```

But here's the thing: That COBOL program will run identically on a mainframe from 1985, a Linux server from 2010, and a cloud container in 2025. Try that with your JavaScript.

### The Bottom Line

COBOL matters because it runs the systems that matter. It's not trendy, it's not exciting, and it definitely won't impress anyone at a tech meetup. But when you need to process millions of financial transactions with zero room for error, COBOL is there, doing what it's done for 65 years: working.

In the next chapter, we'll write our first real COBOL program. You'll learn about divisions (spoiler: there are four, and you'll use them all), the ritual of the IDENTIFICATION DIVISION, and why COBOL programmers TYPE LIKE THEY'RE ALWAYS SHOUTING.

But first, take a moment to appreciate what you're about to learn. You're not just learning a programming language. You're learning to speak to systems that have been running since before the Internet existed. Systems that will probably still be running when we're all coding in quantum qubits.

Welcome to COBOL. It's not dead. It's not dying. It's just getting started with you.

---

*Next Chapter: [Chapter 2: COBOL Basics for Modern Programmers](chapter-02-cobol-basics.md)*

*"COBOL: Come for the job security, stay for the PROCEDURE DIVISION."*