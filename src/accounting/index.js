#!/usr/bin/env node

/**
 * Student Account Management System - Node.js Implementation
 * 
 * This application is a modernized version of the legacy COBOL system.
 * It preserves the original business logic and data flow while implementing
 * the functionality in JavaScript for better maintainability and extensibility.
 * 
 * Original COBOL Components:
 * - MainProgram (main.cob): Menu-driven user interface
 * - Operations (operations.cob): Business logic for transactions
 * - DataProgram (data.cob): Data persistence and retrieval
 * 
 * Consolidated into: index.js
 */

import promptSync from 'prompt-sync';

// ============================================================================
// DATA LAYER - Equivalent to DataProgram (data.cob)
// ============================================================================

/**
 * AccountDataStore - Handles persistent storage and retrieval of account data
 * Equivalent to the DataProgram COBOL module
 */
class AccountDataStore {
  constructor() {
    // STORAGE-BALANCE equivalent: Persistent storage of account balance
    // Initialized to $1000.00 as per business requirements
    this.storageBalance = 1000.00;
  }

  /**
   * READ operation: Retrieves current stored balance
   * @returns {number} The current account balance
   */
  read() {
    return this.storageBalance;
  }

  /**
   * WRITE operation: Updates the stored balance with new value
   * @param {number} balance - The new balance to store
   */
  write(balance) {
    this.storageBalance = balance;
  }

  /**
   * Get current balance (convenience method)
   * @returns {number} The current account balance
   */
  getBalance() {
    return this.read();
  }

  /**
   * Set balance (convenience method)
   * @param {number} balance - The balance to set
   */
  setBalance(balance) {
    this.write(balance);
  }

  /**
   * Reset balance to initial value ($1000.00)
   */
  reset() {
    this.storageBalance = 1000.00;
  }
}

// ============================================================================
// BUSINESS LOGIC LAYER - Equivalent to Operations (operations.cob)
// ============================================================================

/**
 * AccountOperations - Processes all account transactions
 * Equivalent to the Operations COBOL module
 * 
 * Business Rules:
 * - Initial balance: $1000.00
 * - Credit: Always accepted, no upper limit
 * - Debit: Only accepted if sufficient funds available
 * - Overdraft Prevention: No negative balances allowed
 * - Precision: All amounts maintain 2 decimal places
 */
class AccountOperations {
  constructor(dataStore) {
    this.dataStore = dataStore;
  }

  /**
   * TOTAL operation: Display current account balance
   * Equivalent to: IF OPERATION-TYPE = 'TOTAL'
   * @returns {string} Formatted balance message
   */
  viewBalance() {
    const balance = this.dataStore.read();
    return `Current balance: $${balance.toFixed(2)}`;
  }

  /**
   * CREDIT operation: Add amount to account balance
   * Equivalent to: IF OPERATION-TYPE = 'CREDIT'
   * 
   * Process:
   * 1. Accept credit amount from user
   * 2. Read current balance
   * 3. Add credit amount to balance
   * 4. Write updated balance to storage
   * 
   * @param {number} amount - The amount to credit
   * @returns {string} Result message with new balance
   */
  creditAccount(amount) {
    // Validate amount
    if (amount < 0) {
      return "Error: Credit amount cannot be negative.";
    }

    // Read current balance from storage
    const currentBalance = this.dataStore.read();

    // Add credit amount to balance
    const newBalance = currentBalance + amount;

    // Write updated balance to storage
    this.dataStore.write(newBalance);

    // Return success message
    return `Amount credited. New balance: $${newBalance.toFixed(2)}`;
  }

  /**
   * DEBIT operation: Subtract amount from account balance
   * Equivalent to: IF OPERATION-TYPE = 'DEBIT'
   * 
   * Process:
   * 1. Accept debit amount from user
   * 2. Read current balance
   * 3. Validate sufficient funds
   * 4. If funds available:
   *    - Subtract amount and update storage
   * 5. If insufficient funds:
   *    - Display error and cancel transaction
   * 
   * Business Rule: Prevents overdraft (no negative balances)
   * 
   * @param {number} amount - The amount to debit
   * @returns {object} Result object { success: boolean, message: string, balance?: number }
   */
  debitAccount(amount) {
    // Validate amount
    if (amount < 0) {
      return {
        success: false,
        message: "Error: Debit amount cannot be negative."
      };
    }

    // Read current balance from storage
    const currentBalance = this.dataStore.read();

    // Validate sufficient funds (overdraft prevention)
    if (currentBalance < amount) {
      return {
        success: false,
        message: "Insufficient funds for this debit."
      };
    }

    // Subtract amount from balance
    const newBalance = currentBalance - amount;

    // Write updated balance to storage
    this.dataStore.write(newBalance);

    // Return success message
    return {
      success: true,
      message: `Amount debited. New balance: $${newBalance.toFixed(2)}`,
      balance: newBalance
    };
  }
}

// ============================================================================
// PRESENTATION LAYER - Equivalent to MainProgram (main.cob)
// ============================================================================

/**
 * AccountManagementUI - Menu-driven user interface
 * Equivalent to the MainProgram COBOL module
 * 
 * Features:
 * - Interactive menu with 4 options
 * - User input validation
 * - Menu loop until exit
 * - Clear display formatting
 */
class AccountManagementUI {
  constructor(operations) {
    this.operations = operations;
    this.prompt = promptSync();
    this.continueFlag = true;
  }

  /**
   * Display the main menu
   */
  displayMenu() {
    console.clear();
    console.log("--------------------------------");
    console.log("Account Management System");
    console.log("1. View Balance");
    console.log("2. Credit Account");
    console.log("3. Debit Account");
    console.log("4. Exit");
    console.log("--------------------------------");
  }

  /**
   * Handle View Balance operation (Option 1)
   * Equivalent to: EVALUATE USER-CHOICE WHEN 1
   */
  handleViewBalance() {
    const message = this.operations.viewBalance();
    console.log("\n" + message);
    this.prompt("Press Enter to continue...");
  }

  /**
   * Handle Credit Account operation (Option 2)
   * Equivalent to: EVALUATE USER-CHOICE WHEN 2
   */
  handleCreditAccount() {
    console.log("\nEnter credit amount: ");
    const amount = parseFloat(this.prompt("$ "));

    if (isNaN(amount)) {
      console.log("Error: Invalid amount entered.");
      this.prompt("Press Enter to continue...");
      return;
    }

    const message = this.operations.creditAccount(amount);
    console.log(message);
    this.prompt("Press Enter to continue...");
  }

  /**
   * Handle Debit Account operation (Option 3)
   * Equivalent to: EVALUATE USER-CHOICE WHEN 3
   */
  handleDebitAccount() {
    console.log("\nEnter debit amount: ");
    const amount = parseFloat(this.prompt("$ "));

    if (isNaN(amount)) {
      console.log("Error: Invalid amount entered.");
      this.prompt("Press Enter to continue...");
      return;
    }

    const result = this.operations.debitAccount(amount);
    console.log(result.message);
    this.prompt("Press Enter to continue...");
  }

  /**
   * Handle Exit operation (Option 4)
   * Equivalent to: EVALUATE USER-CHOICE WHEN 4
   */
  handleExit() {
    console.log("Exiting the program. Goodbye!");
    this.continueFlag = false;
  }

  /**
   * Handle invalid menu choice
   * Equivalent to: EVALUATE USER-CHOICE WHEN OTHER
   */
  handleInvalidChoice() {
    console.log("Invalid choice, please select 1-4.");
    this.prompt("Press Enter to continue...");
  }

  /**
   * Process user menu selection
   * Equivalent to: EVALUATE USER-CHOICE
   * @param {string} choice - The user's menu selection
   */
  processMenuChoice(choice) {
    switch (choice) {
      case "1":
        this.handleViewBalance();
        break;
      case "2":
        this.handleCreditAccount();
        break;
      case "3":
        this.handleDebitAccount();
        break;
      case "4":
        this.handleExit();
        break;
      default:
        this.handleInvalidChoice();
    }
  }

  /**
   * Main application loop
   * Equivalent to: PERFORM UNTIL CONTINUE-FLAG = 'NO'
   */
  run() {
    while (this.continueFlag) {
      this.displayMenu();
      console.log("Enter your choice (1-4): ");
      const userChoice = this.prompt(">> ");
      this.processMenuChoice(userChoice);
    }
  }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

/**
 * Initialize and run the Student Account Management System
 * 
 * Architecture mimics COBOL data flow:
 * 1. User Input (MainProgram)
 *    ↓
 * 2. Business Logic (Operations)
 *    ↓
 * 3. Data Storage (DataProgram)
 */
function main() {
  // Initialize data layer (equivalent to DataProgram)
  const dataStore = new AccountDataStore();

  // Initialize business logic layer (equivalent to Operations)
  const operations = new AccountOperations(dataStore);

  // Initialize presentation layer (equivalent to MainProgram)
  const ui = new AccountManagementUI(operations);

  // Start the application
  ui.run();
}

// Run the application
main();
