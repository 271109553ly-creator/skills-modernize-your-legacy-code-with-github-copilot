# Student Account Management System - Test Plan

## Overview
This test plan covers the business logic and functionality of the Student Account Management System COBOL application. It includes test cases for menu navigation, account balance inquiries, credit transactions, debit transactions, and system exit functionality.

---

## Test Cases

### Menu Navigation & Input Validation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-001 | Display main menu on application start | Application launched | 1. Execute the application | Menu displays with 4 options (1-View Balance, 2-Credit Account, 3-Debit Account, 4-Exit) | | | |
| TC-002 | Accept valid menu choice (1) | Main menu displayed | 1. Enter "1" at the menu prompt | System accepts input and proceeds to View Balance operation | | | |
| TC-003 | Accept valid menu choice (2) | Main menu displayed | 1. Enter "2" at the menu prompt | System accepts input and proceeds to Credit Account operation | | | |
| TC-004 | Accept valid menu choice (3) | Main menu displayed | 1. Enter "3" at the menu prompt | System accepts input and proceeds to Debit Account operation | | | |
| TC-005 | Accept valid menu choice (4) | Main menu displayed | 1. Enter "4" at the menu prompt | System displays "Exiting the program. Goodbye!" and terminates | | | |
| TC-006 | Reject invalid menu choice (0) | Main menu displayed | 1. Enter "0" at the menu prompt | Error message displays: "Invalid choice, please select 1-4." Menu redisplays | | | |
| TC-007 | Reject invalid menu choice (5) | Main menu displayed | 1. Enter "5" at the menu prompt | Error message displays: "Invalid choice, please select 1-4." Menu redisplays | | | |
| TC-008 | Reject invalid menu choice (non-numeric) | Main menu displayed | 1. Enter "ABC" at the menu prompt | System rejects input or displays error message, menu redisplays | | | |
| TC-009 | Menu loop continues after valid operation | Operation completed | 1. Complete any operation (1, 2, or 3) | Main menu redisplays for next selection | | | |

---

### View Balance Tests (Operation 1)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-010 | View initial account balance | Application started, no prior transactions | 1. Select option 1 (View Balance) | System displays "Current balance: 1000.00" | | | Initial balance is $1000.00 |
| TC-011 | View balance after credit transaction | Account balance is $1000.00 | 1. Perform credit of $500 2. Select option 1 | System displays "Current balance: 1500.00" | | | Balance reflects latest credit |
| TC-012 | View balance after debit transaction | Account balance is $1500.00 | 1. Perform debit of $300 2. Select option 1 | System displays "Current balance: 1200.00" | | | Balance reflects latest debit |
| TC-013 | View balance after multiple transactions | Account balance is $1000.00 | 1. Credit $200 2. Debit $100 3. Credit $150 4. Select option 1 | System displays "Current balance: 1250.00" | | | Balance is cumulative of all transactions |

---

### Credit Account Tests (Operation 2)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-014 | Credit small amount | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 100.00 | Display "Amount credited. New balance: 1100.00" | | | Basic credit functionality |
| TC-015 | Credit large amount | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 999999.99 | Display "Amount credited. New balance: 1000999.99" | | | Maximum credit amount |
| TC-016 | Credit minimum amount ($0.01) | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 0.01 | Display "Amount credited. New balance: 1000.01" | | | Minimum credit amount |
| TC-017 | Credit zero amount ($0.00) | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 0.00 | Display "Amount credited. New balance: 1000.00" (no change) | | | Zero credit should not error |
| TC-018 | Credit exactly $1000 | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 1000.00 | Display "Amount credited. New balance: 2000.00" | | | Large credit amount |
| TC-019 | Multiple consecutive credits | Account balance: $1000.00 | 1. Credit $500 2. Credit $250 3. Credit $100 | Display final balance: 1850.00 | | | Credits accumulate correctly |
| TC-020 | Credit amount with decimals | Account balance: $1000.00 | 1. Select option 2 2. Enter amount: 123.45 | Display "Amount credited. New balance: 1123.45" | | | Decimal handling (2 places) |

---

### Debit Account Tests (Operation 3)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-021 | Debit amount less than balance | Account balance: $1000.00 | 1. Select option 3 2. Enter amount: 100.00 | Display "Amount debited. New balance: 900.00" | | | Basic debit functionality |
| TC-022 | Debit amount equal to balance | Account balance: $1000.00 | 1. Select option 3 2. Enter amount: 1000.00 | Display "Amount debited. New balance: 0.00" | | | Debit entire balance |
| TC-023 | Debit amount greater than balance | Account balance: $500.00 | 1. Select option 3 2. Enter amount: 600.00 | Display "Insufficient funds for this debit." Balance remains: 500.00 | | | Overdraft prevention |
| TC-024 | Debit minimum amount ($0.01) | Account balance: $1000.00 | 1. Select option 3 2. Enter amount: 0.01 | Display "Amount debited. New balance: 999.99" | | | Minimum debit amount |
| TC-025 | Debit zero amount ($0.00) | Account balance: $1000.00 | 1. Select option 3 2. Enter amount: 0.00 | Display "Amount debited. New balance: 1000.00" (no change) | | | Zero debit should not error |
| TC-026 | Debit with decimal amount | Account balance: $1000.00 | 1. Select option 3 2. Enter amount: 234.56 | Display "Amount debited. New balance: 765.44" | | | Decimal handling (2 places) |
| TC-027 | Debit rejected with insufficient funds (low balance) | Account balance: $50.00 | 1. Select option 3 2. Enter amount: 75.00 | Display "Insufficient funds for this debit." Balance remains: 50.00 | | | Validates available balance |
| TC-028 | Multiple consecutive debits | Account balance: $1000.00 | 1. Debit $200 2. Debit $150 3. Debit $100 | Display final balance: 550.00 | | | Multiple debits accumulate |
| TC-029 | Debit after credit | Account balance: $1000.00 | 1. Credit $500 (balance: 1500) 2. Debit $700 | Display "Amount debited. New balance: 800.00" | | | Debit reflects previous credit |

---

### Combined Transaction Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-030 | Credit, then view balance | Account balance: $1000.00 | 1. Credit $300 2. View balance | Display "Current balance: 1300.00" | | | Balance persistence |
| TC-031 | Debit, then view balance | Account balance: $1000.00 | 1. Debit $200 2. View balance | Display "Current balance: 800.00" | | | Balance persistence |
| TC-032 | Credit, debit, credit sequence | Account balance: $1000.00 | 1. Credit $500 2. Debit $300 3. Credit $100 | Final balance: 1300.00 | | | Mixed transaction sequence |
| TC-033 | Failed debit does not change balance | Account balance: $200.00 | 1. Attempt debit $300 (fails) 2. View balance | Final balance: 200.00 | | | Failed transaction isolation |
| TC-034 | Balance persists across multiple menu cycles | Account balance: $1000.00 | 1. Credit $300 2. Return to menu 3. View balance | Display "Current balance: 1300.00" | | | Session-level persistence |

---

### Business Rule Validation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-035 | Initial balance is $1000.00 | Fresh application start | 1. Select option 1 (View Balance) | Display "Current balance: 1000.00" | | | Verify initial balance |
| TC-036 | No negative balances allowed (debit) | Account balance: $100.00 | 1. Attempt debit of $150 | Transaction rejected, balance remains $100.00 | | | Overdraft prevention |
| TC-037 | Positive balances only | Various balances | 1. Perform multiple transactions | All balances >= 0 | | | Account cannot go negative |
| TC-038 | Credit accepts all positive amounts | Account balance: $1000.00 | 1. Credit $999999.99 | Transaction succeeds, balance updates | | | No upper limit on credits |
| TC-039 | Balance precision is 2 decimal places | Any transaction | All balance displays | Balance displays with exactly 2 decimal places | | | Currency formatting |
| TC-040 | Each transaction updates storage immediately | Any transaction | 1. Perform credit 2. Perform debit in same session | Both operations see updated balance from previous transaction | | | Real-time storage updates |

---

### Data Type & Format Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-041 | Amount field accepts up to 6 digits + 2 decimals | Application running | 1. Enter amount: 999999.99 | System accepts the 8-character amount | | | PIC 9(6)V99 format |
| TC-042 | Amount field rejects negative values | Application running - Credit operation | 1. Enter amount: -100.00 | System rejects or handles error | | | No negative amounts |
| TC-043 | Menu choice field accepts single digit | Application running | 1. Enter: 1, 2, 3, or 4 | System accepts single digit selection | | | PIC 9 format |
| TC-044 | Operation type string is 6 characters | Internal validation | Current COBOL code uses 'TOTAL ', 'CREDIT', 'DEBIT ' | All operation identifiers are 6 characters (padded with spaces) | | | PIC X(6) format |

---

### Error Handling & Edge Cases

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|-----------------|-----------|-----------------|---------------|--------|----------|
| TC-045 | Handle non-numeric menu input gracefully | Main menu displayed | 1. Enter: "!" or special character | Display error and redisplay menu | | | Input validation |
| TC-046 | Handle empty input for menu choice | Main menu displayed | 1. Press Enter without input | System prompts again or shows error | | | Empty input handling |
| TC-047 | Handle very large amounts (boundary) | Credit operation | 1. Enter: 999999.99 | Transaction succeeds with maximum value | | | Upper boundary test |
| TC-048 | Handle balance at exactly zero | Account balance: $0.00 | 1. View balance 2. Attempt any debit | Balance shows 0.00, debit is rejected | | | Zero balance handling |
| TC-049 | Re-entry to menu after error | Invalid input provided | 1. Enter invalid menu choice 2. Enter valid choice | Menu loops correctly and accepts valid input | | | Error recovery |

---

## Test Execution Summary

| Test Category | Total Cases | Passed | Failed | Blocked | Pass Rate |
|---------------|-------------|--------|--------|---------|-----------|
| Menu Navigation & Input Validation | 9 | | | | |
| View Balance | 4 | | | | |
| Credit Account | 7 | | | | |
| Debit Account | 9 | | | | |
| Combined Transactions | 5 | | | | |
| Business Rules | 6 | | | | |
| Data Types & Formats | 4 | | | | |
| Error Handling | 5 | | | | |
| **TOTAL** | **49** | | | | |

---

## Notes for Stakeholder Review

### Key Business Rules Validated
1. ✅ Initial account balance of $1000.00
2. ✅ Credit transactions are always accepted (no upper limit)
3. ✅ Debit transactions are only accepted if sufficient funds available
4. ✅ Overdraft prevention - no negative balances
5. ✅ Real-time balance updates and persistence
6. ✅ All monetary values maintain 2 decimal places
7. ✅ Menu-driven user interface

### Test Coverage Areas
- **Functionality**: All four main operations (View, Credit, Debit, Exit)
- **Data Validation**: Input validation for amounts and menu choices
- **Business Logic**: Balance calculations, overdraft prevention
- **Edge Cases**: Zero amounts, maximum amounts, boundary conditions
- **Error Handling**: Invalid inputs, insufficient funds scenarios
- **State Management**: Balance persistence across operations

### Integration Testing Considerations for Node.js Migration
- API endpoints for each operation (GET balance, POST credit, POST debit)
- Request/response format validation
- Database persistence instead of in-memory storage
- Concurrent user support (if needed)
- Audit logging for transactions
- Transaction history tracking

### Recommended Node.js Implementation
Based on this test plan, the Node.js application should include:
- Express.js REST API endpoints
- Unit tests for business logic (Jest/Mocha)
- Integration tests for API endpoints
- Database schema for accounts and transactions
- Input validation middleware
- Error handling middleware
- Transaction logging/audit trail

---

## Sign-Off

**Prepared By**: AI Assistant
**Date**: February 16, 2026
**Status**: Ready for Stakeholder Review and Node.js Implementation
