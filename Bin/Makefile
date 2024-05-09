# Compiler
CC = mpicc

# Compiler flags
CFLAGS = -Wall -Wextra

# Executable name
EXEC = mpi_version

# Source files
SRCS = mpi_version.c

# Object files
OBJS = $(SRCS:.c=.o)

# Target: mpi_version
$(EXEC): $(OBJS)
	@$(CC) $(CFLAGS) -o $@ $(OBJS)

# Rule for compiling .c files to .o files
%.o: %.c
	@$(CC) $(CFLAGS) -c $< -o $@

