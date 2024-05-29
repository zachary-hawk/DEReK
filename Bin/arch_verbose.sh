#!/bin/sh

# Function to get the number of physical cores
get_physical_cores() {
    if [ "$(uname)" = "Linux" ]; then
        grep -c '^processor' /proc/cpuinfo
    elif [ "$(uname)" = "Darwin" ]; then
        sysctl -n hw.physicalcpu
    fi
}

# Function to get the number of logical cores
get_logical_cores() {
    if [ "$(uname)" = "Linux" ]; then
        grep -c '^processor' /proc/cpuinfo
    elif [ "$(uname)" = "Darwin" ]; then
        sysctl -n hw.logicalcpu
    fi
}

# Function to get the total amount of system memory
get_total_memory() {
    if [ "$(uname)" = "Linux" ]; then
        awk '/MemTotal/ {print $2}' /proc/meminfo
    elif [ "$(uname)" = "Darwin" ]; then
        sysctl -n hw.memsize
    fi
}

# Convert memory from bytes to GB
convert_to_gb() {
    echo "scale=2; $1 / 1024 / 1024 / 1024" | bc
}

physical_cores=$(get_physical_cores)
logical_cores=$(get_logical_cores)
total_memory=$(get_total_memory)
total_memory_gb=$(convert_to_gb $total_memory)

echo "$physical_cores"
echo "$logical_cores"
echo "${total_memory_gb}"
