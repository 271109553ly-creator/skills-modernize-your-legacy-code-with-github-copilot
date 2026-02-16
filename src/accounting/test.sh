#!/bin/bash

# Test Runner Script for Student Account Management System
# Simplifies running Jest tests with proper Node.js ES module configuration

export NODE_OPTIONS=--experimental-vm-modules
exec jest "$@"
