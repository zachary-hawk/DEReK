# Compiler
CC = mpicc

# Compiler flags
CFLAGS = -Wall -Wextra -Wno-coverage-mismatch -Wno-implicit -w

# Executable name
EXEC = mpi_version

# Source files
SRCS = mpi_version.c

# Object files
OBJS = $(SRCS:.c=.o)

# Target: mpi_version
$(EXEC): $(OBJS)
	@echo 'Building mpi_version'
	@$(CC) $(CFLAGS) -o $@ $(OBJS)

# Rule for compiling .c files to .o files
%.o: %.c
	@$(CC) $(CFLAGS) -c $< -o $@

