#!/bin/bash

# Function to check for fftw3 using pkg-config
check_fftw3_with_pkgconfig() {
    if command -v pkg-config &>/dev/null; then
        echo "Checking for fftw3 using pkg-config..."
        if pkg-config --exists fftw3; then
            echo "fftw3 is installed."
            echo "Library path: $(pkg-config --libs fftw3)"
            echo "Include path: $(pkg-config --cflags fftw3)"
        else
            echo "fftw3 not found using pkg-config."
        fi
    else
        echo "pkg-config is not installed or not in PATH. Skipping pkg-config check."
    fi
}

# Function to search for fftw3 using common paths
check_fftw3_with_find() {
    echo "Searching for fftw3 library files in common paths..."
    fftw3_paths=$(find /usr /opt /lib /usr/local -type f -name "libfftw3*.so*" -o -name "libfftw3*.dylib" 2>/dev/null)
    
    if [ -n "$fftw3_paths" ]; then
        echo "fftw3 libraries found at:"
        echo "$fftw3_paths"
    else
        echo "fftw3 libraries not found in common paths."
    fi
}

# Function to search for fftw3 header files
check_fftw3_headers() {
    echo "Searching for fftw3 header files in common paths..."
    fftw3_headers=$(find /usr /opt /usr/local/include -type f -name "fftw3.h" 2>/dev/null)
    
    if [ -n "$fftw3_headers" ]; then
        echo "fftw3 headers found at:"
        echo "$fftw3_headers"
    else
        echo "fftw3 headers not found in common paths."
    fi
}

# Main execution
echo "Checking for fftw3 library on the system..."
check_fftw3_with_pkgconfig
check_fftw3_with_find
check_fftw3_headers

echo "Check completed."
