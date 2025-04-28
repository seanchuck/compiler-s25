#!/bin/bash
# compiles all decaf files into executables in /local/exec
# intermediates (.s and .debug) are stored in /local/intermediates

# Set the base directories
BASE_DIR="tests/phase3"
PHASE3_INPUT_DIR="$BASE_DIR/input"
PHASE4_DIR="tests/phase4"
LOCAL_DIR="local"
INTERMEDIATE_DIR="$LOCAL_DIR/intermediates"
EXECUTABLE_DIR="$LOCAL_DIR/exec"

# ========== Parse arguments ==========
OPT_FLAG=""

while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -O)
        if [[ -n "$2" ]]; then
            OPT_FLAG="-O $2"
            shift
        fi
        shift
        ;;
        *)
        echo "Unknown argument: $1"
        echo "Usage: $0 [-O optimization_level]"
        exit 1
        ;;
    esac
done

# ========== Make sure local/ directories exist ==========
mkdir -p "$INTERMEDIATE_DIR"
mkdir -p "$EXECUTABLE_DIR"

# ========== Clean local/ folders ==========
echo "Cleaning old intermediates and executables..."
rm -f "$INTERMEDIATE_DIR"/*
rm -f "$EXECUTABLE_DIR"/*

# ========== Helper function to compile one file ==========
compile_file() {
    local file="$1"
    local filename="$(basename -- "$file")"    # e.g., foo.dcf
    local stem="${filename%.*}"                # e.g., foo
    local assembly_file="$INTERMEDIATE_DIR/$stem.s"
    local executable_file="$EXECUTABLE_DIR/$stem.out"
    local debug_output="$INTERMEDIATE_DIR/$stem.debug"

    echo "========================================"
    echo "Compiling source: $file"

    # Determine optimization to display
    local opt_display="none"
    if [[ -n "$OPT_FLAG" ]]; then
        opt_display="$(echo "$OPT_FLAG" | awk '{print $2}')"
    fi
    echo "Optimization: [$opt_display]"

    # Run your compiler, writing output into intermediates/
    echo "./run.sh -t assembly \"$file\" $OPT_FLAG -o \"$assembly_file\""
    ./run.sh -t assembly "$file" $OPT_FLAG -o "$assembly_file" > "$debug_output"

    if [[ $? -ne 0 ]]; then
        echo "❌ Failed to compile $file with run.sh"
        return
    fi

    # Check if .s file was generated
    if [[ ! -f "$assembly_file" ]]; then
        echo "❌ Assembly file not generated: $assembly_file"
        return
    fi

    # Assemble and link
    echo "gcc -O0 -no-pie \"$assembly_file\" -o \"$executable_file\""
    gcc -O0 -no-pie "$assembly_file" -o "$executable_file"

    if [[ $? -ne 0 ]]; then
        echo "❌ Failed to assemble $assembly_file with gcc"
        return
    fi

    echo "✅ Successfully compiled $file --> $executable_file"
}

# ========== Compile all phase3/input .dcf files ==========
for file in "$PHASE3_INPUT_DIR"/*.dcf; do
    if [[ -f "$file" ]]; then
        compile_file "$file"
    fi
done

# ========== Compile all phase4 .dcf files ==========
for file in "$PHASE4_DIR"/*.dcf; do
    if [[ -f "$file" ]]; then
        compile_file "$file"
    fi
done

echo "Compilation finished."
