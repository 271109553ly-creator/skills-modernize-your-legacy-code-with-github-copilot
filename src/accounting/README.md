# Node.js Application - Quick Start Guide

## Project Overview
The Student Account Management System has been successfully modernized from COBOL to Node.js, preserving all original business logic and data flow.

## Directory Structure

```
src/accounting/
├── index.js              # Main application (450+ lines, fully documented)
├── package.json          # Node.js dependencies
├── package-lock.json     # Locked dependency versions
└── node_modules/         # Installed packages
```

## Prerequisites

✅ Node.js >= 14.0.0  
✅ npm (comes with Node.js)  
✅ All dependencies installed

## Running the Application

### Method 1: Direct Execution (Terminal)

```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm start
```

Output:
```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4):
```

### Method 2: VSCode Debugging

1. Open `.vscode/launch.json` (already configured)
2. Press `F5` or go to Run → Start Debugging
3. Select "Launch Accounting App" from dropdown
4. Application will start in integrated terminal

### Method 3: Run with Inspector (Remote Debugging)

```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm run dev
```

Then open: `chrome://inspect` in browser to attach debugger

## Application Architecture

### Three-Tier Architecture (Preserved from COBOL)

```
┌─────────────────────────────┐
│   AccountManagementUI       │  ← Presentation Layer (MainProgram equivalent)
│  - Menu Display             │
│  - User Input Handling      │
│  - Choice Routing           │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│   AccountOperations         │  ← Business Logic Layer (Operations equivalent)
│  - viewBalance()            │
│  - creditAccount()          │
│  - debitAccount()           │
│  - Overdraft Prevention     │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│  AccountDataStore           │  ← Data Layer (DataProgram equivalent)
│  - storageBalance           │
│  - read() / write()         │
│  - Session Persistence      │
└─────────────────────────────┘
```

## Classes and Methods

### 1. AccountDataStore
**Purpose:** Data persistence layer (equivalent to COBOL DataProgram)

```javascript
class AccountDataStore {
  read()                    // Get current balance
  write(balance)            // Update balance
  getBalance()              // Convenience getter
  setBalance(balance)       // Convenience setter
  reset()                   // Reset to $1000.00
}
```

### 2. AccountOperations
**Purpose:** Business logic (equivalent to COBOL Operations)

```javascript
class AccountOperations {
  viewBalance()             // Display current balance
  creditAccount(amount)     // Add funds
  debitAccount(amount)      // Subtract funds (with validation)
}
```

### 3. AccountManagementUI
**Purpose:** User interface (equivalent to COBOL MainProgram)

```javascript
class AccountManagementUI {
  displayMenu()             // Show menu options
  handleViewBalance()       // Option 1
  handleCreditAccount()     // Option 2
  handleDebitAccount()      // Option 3
  handleExit()              // Option 4
  processMenuChoice()       // Route user selection
  run()                     // Main application loop
}
```

## Business Logic Preserved

### Initial Balance
- All accounts start with **$1000.00**

### Credit Operations
- **Always accepted**
- No upper limit
- Updates balance immediately
- Maintains 2 decimal place precision

### Debit Operations
- **Only accepted if sufficient funds**
- Maximum: current balance
- **Overdraft prevented** - rejects transactions that would make balance negative
- Updates balance only if successful

### Data Management
- **In-memory storage** (can be migrated to database)
- **Session-based persistence** (lasts until program exit)
- **No data loss** between operations in same session

## Menu Options

| Option | Function | COBOL Equivalent |
|--------|----------|------------------|
| 1 | View Balance | TOTAL operation |
| 2 | Credit Account | CREDIT operation |
| 3 | Debit Account | DEBIT operation |
| 4 | Exit | STOP RUN |

## Testing

### Test Plan Available
See [docs/TESTPLAN.md](../docs/TESTPLAN.md) for:
- 49 comprehensive test cases
- All business logic coverage
- Menu navigation validation
- Transaction testing
- Edge case handling

### Running Test Cases Manually

Example: Test Case TC-010 (View initial balance)
```
1. Start application
2. Select option: 1
3. Expected: "Current balance: $1000.00"
```

Example: Test Case TC-023 (Debit with insufficient funds)
```
1. Start application
2. Select: 3 (Debit)
3. Enter amount: 1500.00
4. Expected: "Insufficient funds for this debit."
5. Select: 1 (View Balance)
6. Expected: "Current balance: $1000.00" (unchanged)
```

## File Descriptions

### index.js (450+ lines)
**Complete application** with three classes:
- ✅ Fully commented with COBOL mappings
- ✅ Input validation at all layers
- ✅ Error handling for edge cases
- ✅ Proper object encapsulation
- ✅ ES6 module syntax

### package.json
**Node.js project configuration:**
- Dependencies: prompt-sync (terminal interaction)
- Scripts: start, dev
- Target: Node >= 14.0.0

### .vscode/launch.json
**Debug configuration** with two launch profiles:
1. Standard launch with integrated terminal
2. Debug with Node inspector (port 9229)

## Documentation

| Document | Purpose |
|----------|---------|
| [README.md](../docs/README.md) | Detailed system documentation & sequence diagram |
| [TESTPLAN.md](../docs/TESTPLAN.md) | 49 test cases for validation |
| [MIGRATION_SUMMARY.md](../docs/MIGRATION_SUMMARY.md) | COBOL→Node.js mapping details |

## Troubleshooting

### Issue: "prompt-sync not found"
**Solution:**
```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm install
```

### Issue: "Module not found: prompt-sync"
**Solution:**
```bash
# Make sure you're in the accounting directory
cd src/accounting
npm install
```

### Issue: Cannot read input (hanging)
**Solution:**
- Make sure you're using VSCode integrated terminal or a proper terminal
- Don't use pipes or background execution
- Run directly: `npm start`

## Next Steps for Enhancement

1. **Add Unit Tests**
   ```bash
   npm install --save-dev jest
   npm test
   ```

2. **Add Data Persistence**
   - Replace in-memory storage with SQLite
   - Add transaction history

3. **Build REST API**
   - Add Express.js
   - Create HTTP endpoints
   - Enable API clients to access accounts

4. **Create Web UI**
   - Add React/Vue frontend
   - Replace terminal menu with web interface

5. **Containerization**
   - Create Dockerfile
   - Setup Docker Compose
   - Deploy to cloud

## Quick Commands Reference

```bash
# Install dependencies
cd src/accounting && npm install

# Run application
npm start

# Run with debugger
npm run dev

# Check syntax
node --check index.js

# Run tests (once added)
npm test

# Build for production (once configured)
npm run build
```

## Support & Documentation

- Original COBOL source: [src/cobol/](../cobol/)
- System documentation: [docs/README.md](../docs/README.md)
- Test plan: [docs/TESTPLAN.md](../docs/TESTPLAN.md)
- Migration details: [docs/MIGRATION_SUMMARY.md](../docs/MIGRATION_SUMMARY.md)

---

**Status:** ✅ Ready for development, testing, and deployment  
**Last Updated:** February 16, 2026
