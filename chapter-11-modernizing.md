# Chapter 11: Modernizing Legacy COBOL
## Making the 1960s Talk to the 2020s

---

### The Modernization Dilemma

You have a COBOL system that's been running since 1975. It processes millions of transactions daily. It has never had a data breach. It has 10 million lines of code. Your CEO wants it "in the cloud" by next quarter.

Welcome to COBOL modernization, where "if it ain't broke" meets "digital transformation."

### The Strategies (From Least to Most Terrifying)

1. **Encapsulation**: Wrap it in APIs
2. **Extension**: Add modern capabilities
3. **Renovation**: Refactor and restructure
4. **Migration**: Move to modern platforms
5. **Replacement**: Rewrite everything (good luck)

### Strategy 1: API Wrapping - The Safest Bet

```cobol
      *================================================================
      * RESTAPI.COB - RESTful API wrapper for legacy COBOL
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESTAPI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-REQUEST.
           05 WS-METHOD        PIC X(10).
           05 WS-ENDPOINT      PIC X(100).
           05 WS-JSON-INPUT    PIC X(1000).

       01  WS-RESPONSE.
           05 WS-STATUS-CODE   PIC 999.
           05 WS-JSON-OUTPUT   PIC X(5000).

       LINKAGE SECTION.
       01  LS-HTTP-REQUEST   PIC X(8000).
       01  LS-HTTP-RESPONSE  PIC X(8000).

       PROCEDURE DIVISION USING LS-HTTP-REQUEST
                                LS-HTTP-RESPONSE.
       MAIN-PROCESS.
           * Parse HTTP request
           PERFORM PARSE-REQUEST.

           * Route to appropriate handler
           EVALUATE WS-ENDPOINT
               WHEN "/api/customer"
                   PERFORM HANDLE-CUSTOMER-API
               WHEN "/api/account"
                   PERFORM HANDLE-ACCOUNT-API
               WHEN "/api/transaction"
                   PERFORM HANDLE-TRANSACTION-API
               WHEN OTHER
                   MOVE 404 TO WS-STATUS-CODE
                   MOVE '{"error":"Not found"}' TO WS-JSON-OUTPUT
           END-EVALUATE.

           * Build HTTP response
           PERFORM BUILD-RESPONSE.
           EXIT PROGRAM.

       HANDLE-CUSTOMER-API.
           EVALUATE WS-METHOD
               WHEN "GET"
                   PERFORM GET-CUSTOMER
               WHEN "POST"
                   PERFORM CREATE-CUSTOMER
               WHEN "PUT"
                   PERFORM UPDATE-CUSTOMER
               WHEN "DELETE"
                   PERFORM DELETE-CUSTOMER
           END-EVALUATE.

       GET-CUSTOMER.
           * Parse JSON to get customer ID
           JSON PARSE WS-JSON-INPUT INTO WS-CUSTOMER-ID.

           * Call legacy program
           CALL 'CUSTINQ' USING WS-CUSTOMER-ID
                                WS-CUSTOMER-DATA
                                WS-RETURN-CODE.

           * Convert to JSON
           JSON GENERATE WS-JSON-OUTPUT FROM WS-CUSTOMER-DATA.
           MOVE 200 TO WS-STATUS-CODE.
```

### Web Services with COBOL

```cobol
       * SOAP Web Service
       01  WS-SOAP-REQUEST.
           05 WS-SOAP-ENVELOPE.
              10 WS-SOAP-HEADER    PIC X(500).
              10 WS-SOAP-BODY      PIC X(2000).

       01  WS-WSDL-URL           PIC X(200).

       CALL-SOAP-SERVICE.
           * Build SOAP envelope
           STRING '<?xml version="1.0"?>'
                  '<soap:Envelope xmlns:soap='
                  '"http://www.w3.org/2003/05/soap-envelope">'
                  '<soap:Body>'
                  WS-REQUEST-XML
                  '</soap:Body>'
                  '</soap:Envelope>'
               INTO WS-SOAP-REQUEST.

           * Call web service
           CALL 'CBLTWS' USING WS-WSDL-URL
                               WS-SOAP-REQUEST
                               WS-SOAP-RESPONSE
                               WS-HTTP-STATUS.

       * REST with JSON
       01  WS-REST-ENDPOINT      PIC X(200).
       01  WS-JSON-REQUEST       PIC X(1000).
       01  WS-JSON-RESPONSE      PIC X(5000).

       CALL-REST-SERVICE.
           * Build JSON request
           JSON GENERATE WS-JSON-REQUEST FROM WS-REQUEST-DATA
               COUNT IN WS-JSON-LENGTH.

           * Set headers
           MOVE "application/json" TO WS-CONTENT-TYPE.
           MOVE WS-AUTH-TOKEN TO WS-AUTHORIZATION.

           * Make HTTP call
           CALL 'HTTPPOST' USING WS-REST-ENDPOINT
                                WS-JSON-REQUEST
                                WS-JSON-RESPONSE
                                WS-HTTP-STATUS.

           * Parse response
           JSON PARSE WS-JSON-RESPONSE INTO WS-RESPONSE-DATA.
```

### Containerizing COBOL

```dockerfile
# Dockerfile for COBOL application
FROM ubuntu:20.04

# Install GnuCOBOL
RUN apt-get update && \
    apt-get install -y gnucobol libcob4 libcob4-dev

# Copy COBOL programs
COPY *.cob /app/
COPY *.cpy /app/copybooks/

# Compile programs
WORKDIR /app
RUN cobc -x -o customer_service customer.cob
RUN cobc -x -o account_service account.cob

# Install API layer (Node.js example)
RUN apt-get install -y nodejs npm
COPY package.json /app/
RUN npm install

# Copy API wrapper
COPY api-server.js /app/

# Expose port
EXPOSE 3000

# Start API server
CMD ["node", "api-server.js"]
```

### Kubernetes Deployment

```yaml
# cobol-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: legacy-cobol-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: cobol-api
  template:
    metadata:
      labels:
        app: cobol-api
    spec:
      containers:
      - name: cobol-service
        image: mycompany/cobol-api:latest
        ports:
        - containerPort: 3000
        env:
        - name: DB_CONNECTION
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: connection-string
---
apiVersion: v1
kind: Service
metadata:
  name: cobol-api-service
spec:
  selector:
    app: cobol-api
  ports:
  - port: 80
    targetPort: 3000
  type: LoadBalancer
```

### Microservices Extraction

```cobol
      *================================================================
      * Extract business logic into microservice
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAX-MICROSERVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MQ-CONNECTION    PIC X(8).
       01  WS-QUEUE-NAME       PIC X(48) VALUE 'TAX.CALCULATION.QUEUE'.
       01  WS-MESSAGE          PIC X(1000).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           * Connect to message queue
           CALL 'MQCONN' USING WS-MQ-CONNECTION.

           * Main processing loop
           PERFORM UNTIL WS-SHUTDOWN = 'Y'
               * Get message from queue
               CALL 'MQGET' USING WS-MQ-CONNECTION
                                  WS-QUEUE-NAME
                                  WS-MESSAGE
                                  WS-RETURN-CODE

               IF WS-RETURN-CODE = 0
                   PERFORM PROCESS-TAX-REQUEST
               ELSE
                   CALL 'CBL_OC_NANOSLEEP' USING 1000000000
               END-IF
           END-PERFORM.

           * Disconnect
           CALL 'MQDISC' USING WS-MQ-CONNECTION.

       PROCESS-TAX-REQUEST.
           * Parse request
           JSON PARSE WS-MESSAGE INTO WS-TAX-REQUEST.

           * Call legacy tax calculation
           CALL 'TAXCALC' USING WS-TAX-REQUEST
                                WS-TAX-RESPONSE.

           * Send response
           JSON GENERATE WS-RESPONSE-MESSAGE FROM WS-TAX-RESPONSE.

           CALL 'MQPUT' USING WS-MQ-CONNECTION
                              'TAX.RESPONSE.QUEUE'
                              WS-RESPONSE-MESSAGE.
```

### Database Modernization

```cobol
       * Gradual migration from files to database
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION USE-DATABASE-FLAG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-USE-DATABASE     PIC X VALUE 'N'.
           88 USE-DATABASE     VALUE 'Y'.
           88 USE-FILE         VALUE 'N'.

       PROCEDURE DIVISION.
       READ-CUSTOMER.
           * Check feature flag
           ACCEPT WS-USE-DATABASE FROM ENVIRONMENT "USE_DATABASE".

           IF USE-DATABASE
               PERFORM READ-FROM-DATABASE
           ELSE
               PERFORM READ-FROM-FILE
           END-IF.

       READ-FROM-DATABASE.
           EXEC SQL
               SELECT * INTO :WS-CUSTOMER
               FROM CUSTOMERS
               WHERE CUST_ID = :WS-CUSTOMER-ID
           END-EXEC.

       READ-FROM-FILE.
           READ CUSTOMER-FILE
               KEY IS CUST-ID
               INVALID KEY
                   MOVE 'NOT-FOUND' TO WS-STATUS.

       * Dual-write pattern for migration
       WRITE-CUSTOMER.
           * Always write to file (legacy)
           WRITE CUSTOMER-RECORD.

           * Also write to database if enabled
           IF USE-DATABASE
               EXEC SQL
                   INSERT INTO CUSTOMERS
                   VALUES (:WS-CUSTOMER)
               END-EXEC
           END-IF.
```

### Event-Driven Architecture

```cobol
       * Publish events for legacy operations
       01  WS-EVENT-TOPIC      PIC X(50).
       01  WS-EVENT-PAYLOAD    PIC X(1000).

       PROCEDURE DIVISION.
       PROCESS-ORDER.
           * Legacy order processing
           CALL 'ORDPROC' USING WS-ORDER-DATA.

           * Publish event
           MOVE 'order.processed' TO WS-EVENT-TOPIC.
           JSON GENERATE WS-EVENT-PAYLOAD FROM WS-ORDER-DATA.

           CALL 'KAFKA-PUBLISH' USING WS-EVENT-TOPIC
                                      WS-EVENT-PAYLOAD.

       SUBSCRIBE-TO-EVENTS.
           CALL 'KAFKA-SUBSCRIBE' USING 'inventory.updated'.

           PERFORM UNTIL WS-SHUTDOWN = 'Y'
               CALL 'KAFKA-POLL' USING WS-EVENT-MESSAGE
                                       WS-RETURN-CODE

               IF WS-RETURN-CODE = 0
                   PERFORM HANDLE-INVENTORY-EVENT
               END-IF
           END-PERFORM.
```

### GraphQL Integration

```cobol
       * GraphQL resolver in COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAPHQL-RESOLVER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-QUERY            PIC X(500).
       01  WS-RESULT           PIC X(5000).

       LINKAGE SECTION.
       01  LS-GRAPHQL-QUERY    PIC X(1000).
       01  LS-GRAPHQL-RESULT   PIC X(8000).

       PROCEDURE DIVISION USING LS-GRAPHQL-QUERY
                                LS-GRAPHQL-RESULT.
       PARSE-QUERY.
           * Simple GraphQL query parser
           IF LS-GRAPHQL-QUERY(1:8) = "customer"
               PERFORM RESOLVE-CUSTOMER
           ELSE IF LS-GRAPHQL-QUERY(1:7) = "account"
               PERFORM RESOLVE-ACCOUNT
           END-IF.

       RESOLVE-CUSTOMER.
           * Extract requested fields
           * Call legacy programs
           * Build GraphQL response
           CALL 'CUSTINQ' USING WS-PARAMETERS.
           JSON GENERATE LS-GRAPHQL-RESULT FROM WS-CUSTOMER-DATA.
```

### CI/CD for COBOL

```yaml
# .gitlab-ci.yml for COBOL
stages:
  - compile
  - test
  - package
  - deploy

compile-cobol:
  stage: compile
  script:
    - cobc -x -o customer_service src/customer.cob
    - cobc -x -o account_service src/account.cob
  artifacts:
    paths:
      - customer_service
      - account_service

test-cobol:
  stage: test
  script:
    - cobc -x -o test_runner tests/test_suite.cob
    - ./test_runner
    - if [ $? -ne 0 ]; then exit 1; fi

build-docker:
  stage: package
  script:
    - docker build -t cobol-api:$CI_COMMIT_SHA .
    - docker push registry.company.com/cobol-api:$CI_COMMIT_SHA

deploy-k8s:
  stage: deploy
  script:
    - kubectl set image deployment/cobol-api cobol-api=registry.company.com/cobol-api:$CI_COMMIT_SHA
```

### Gradual Refactoring Patterns

```cobol
       * Strangler Fig Pattern - Gradually replace functionality
       PROCESS-PAYMENT.
           * Check feature toggle
           IF NEW-PAYMENT-SYSTEM-ENABLED
               CALL 'NEW-PAYMENT-API' USING WS-PAYMENT-DATA
           ELSE
               PERFORM LEGACY-PAYMENT-PROCESS
           END-IF.

       * Branch by Abstraction
       CALCULATE-DISCOUNT.
           CALL WS-DISCOUNT-IMPLEMENTATION USING WS-PARAMETERS.

       INITIALIZE-SERVICES.
           IF MODERN-RULES-ENGINE-AVAILABLE
               MOVE 'MODERN-DISCOUNT' TO WS-DISCOUNT-IMPLEMENTATION
           ELSE
               MOVE 'LEGACY-DISCOUNT' TO WS-DISCOUNT-IMPLEMENTATION
           END-IF.
```

### Documentation Generation

```cobol
      *> @api {get} /api/customer/:id Get Customer
      *> @apiName GetCustomer
      *> @apiGroup Customer
      *> @apiVersion 1.0.0
      *>
      *> @apiParam {Number} id Customer's unique ID
      *>
      *> @apiSuccess {String} name Customer name
      *> @apiSuccess {Number} balance Account balance
      *>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-CUSTOMER.
```

### Monitoring and Observability

```cobol
       * Add metrics collection
       01  WS-METRICS.
           05 WS-COUNTER-NAME  PIC X(50).
           05 WS-COUNTER-VALUE PIC 9(9).

       PROCEDURE DIVISION.
       PROCESS-TRANSACTION.
           * Increment counter
           MOVE 'transactions.processed' TO WS-COUNTER-NAME.
           MOVE 1 TO WS-COUNTER-VALUE.
           CALL 'METRICS-INC' USING WS-COUNTER-NAME
                                    WS-COUNTER-VALUE.

           * Record timing
           ACCEPT WS-START-TIME FROM TIME.
           PERFORM ACTUAL-PROCESSING.
           ACCEPT WS-END-TIME FROM TIME.

           COMPUTE WS-DURATION = WS-END-TIME - WS-START-TIME.
           CALL 'METRICS-TIMING' USING 'transaction.duration'
                                       WS-DURATION.

           * Add trace
           CALL 'TRACE-SPAN' USING 'transaction.process'
                                   WS-TRACE-ID
                                   WS-SPAN-DATA.
```

### Anti-Patterns to Avoid

```cobol
       * DON'T: Big Bang Rewrite
       * Trying to replace everything at once

       * DON'T: Literal Translation
       * Converting COBOL to Java line-by-line

       * DON'T: Ignoring Business Logic
       * The weird IF statement might be critical

       * DON'T: Breaking Interfaces
       * Other systems depend on exact formats

       * DON'T: Losing Precision
       * COBOL's decimal math is exact, float isn't
```

### The Modernization Roadmap

1. **Assessment Phase**
   - Inventory all programs
   - Map dependencies
   - Identify critical paths
   - Document business rules

2. **Stabilization Phase**
   - Add automated testing
   - Implement CI/CD
   - Add monitoring
   - Create documentation

3. **Encapsulation Phase**
   - Wrap with APIs
   - Add message queues
   - Implement caching
   - Create service layer

4. **Transformation Phase**
   - Extract microservices
   - Migrate to cloud
   - Modernize database
   - Update interfaces

5. **Optimization Phase**
   - Performance tuning
   - Cost optimization
   - Scale horizontally
   - Add resilience

### What You've Learned

Modernizing COBOL isn't about replacing it—it's about making it play nice with modern infrastructure. The code that's processed trillions of dollars doesn't need to be rewritten; it needs to be accessible, scalable, and maintainable.

The key is incremental change. Wrap it, extend it, gradually transform it, but never break it. Because somewhere, someone's paycheck depends on that COBOL program running exactly as it has for the last 40 years.

---

*Next Chapter: [Chapter 12: Real-World Code Review Scenarios](chapter-12-code-review.md)*

*"The best way to modernize COBOL is to stop calling it legacy and start calling it proven."*