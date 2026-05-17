#!/usr/bin/env bash

set -e

# Path to the compiled binary
HACTAR_BIN="./hactar"
RUN_LLM_TESTS=0

# Parse arguments
for arg in "$@"; do
    if [ "$arg" == "--llm" ]; then
        RUN_LLM_TESTS=1
    fi
done

# Ensure the binary exists
if [ ! -f "$HACTAR_BIN" ]; then
    echo "Error: $HACTAR_BIN not found. Please run 'make build' first."
    exit 1
fi

passed=0
failed=0

# Helper function to run a test
run_test() {
    local name="$1"
    local cmd="$2"
    local expect_fail="${3:-0}"

    echo -n "Testing $name... "
    
    # Run the command, capture stdout and stderr
    output=$(eval "$cmd" 2>&1)
    exit_code=$?

    local success=0
    if [ "$expect_fail" -eq 0 ] && [ "$exit_code" -eq 0 ]; then
        success=1
    elif [ "$expect_fail" -eq 1 ] && [ "$exit_code" -ne 0 ]; then
        success=1
    fi

    if [ "$success" -eq 1 ]; then
        echo -e "\033[32mPASS\033[0m"
        passed=$((passed + 1))
    else
        echo -e "\033[31mFAIL\033[0m"
        echo "Command: $cmd"
        echo "Exit code: $exit_code"
        echo "Output: $output"
        failed=$((failed + 1))
    fi
}

echo "========================================"
echo " Running Hactar CLI Integration Tests"
echo "========================================"

# ---------------------------------------------------------
# Basic Flag Tests
# ---------------------------------------------------------
run_test "Flag: --help" "$HACTAR_BIN --help"
run_test "Flag: --version" "$HACTAR_BIN --version"

# ---------------------------------------------------------
# Offline Subcommand Tests
# ---------------------------------------------------------
run_test "Subcommand: help" "$HACTAR_BIN help"
run_test "Subcommand: check" "$HACTAR_BIN check"
run_test "Subcommand: agents" "$HACTAR_BIN agents"
run_test "Subcommand: component.list" "$HACTAR_BIN component.list"
run_test "Subcommand: frameworkroute.list" "$HACTAR_BIN frameworkroute.list"
run_test "Subcommand: lib.list" "$HACTAR_BIN lib.list"
run_test "Subcommand: model.list" "$HACTAR_BIN model.list"
run_test "Subcommand: mold.list" "$HACTAR_BIN mold.list"
run_test "Subcommand: molds" "$HACTAR_BIN molds"
run_test "Subcommand: preset list" "$HACTAR_BIN preset list"
run_test "Subcommand: rules.list" "$HACTAR_BIN rules.list"
run_test "Subcommand: session list" "$HACTAR_BIN session list"
run_test "Subcommand: skills.list" "$HACTAR_BIN skills.list"
run_test "Subcommand: wiki.list" "$HACTAR_BIN wiki.list"

# Test fallback failure for invalid command
run_test "Subcommand: invalid_command" "$HACTAR_BIN some_invalid_command_name_xyz" 1

# ---------------------------------------------------------
# LLM Dependent Tests
# ---------------------------------------------------------
if [ "$RUN_LLM_TESTS" -eq 1 ]; then
    echo "----------------------------------------"
    echo " Running LLM Dependent Tests"
    echo "----------------------------------------"
    
    run_test "Query Flag: -q" "$HACTAR_BIN -q 'Say hello'"
    run_test "Model Override: -m" "$HACTAR_BIN -m openrouter/deepseek/deepseek-chat-v3-0324 -q 'Say hello'"
    run_test "Subcommand: sh!" "$HACTAR_BIN sh\! 'echo hello world'"
else
    echo "----------------------------------------"
    echo " Skipping LLM tests (pass --llm to run)"
    echo "----------------------------------------"
fi

echo "========================================"
echo " Test Summary: $passed passed, $failed failed."
if [ "$failed" -gt 0 ]; then
    exit 1
fi
exit 0
