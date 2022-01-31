BUILD_DIR ?= ./out
SRC_DIRS ?= ./src

CC = gcc -Wall -Werror=implicit-function-declaration -Werror=return-type
ASM = nasm -felf64

SRCS := $(shell find $(SRC_DIRS) -type f -name "*.c" -or -name "*.s")
OBJS := $(SRCS:%.c=$(BUILD_DIR)/%.o) $(SRCS:%.s=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) -c $< -o $@

$(BUILD_DIR)/%.o: %.s
	mkdir -p $(dir $@)
	$(ASM) $< -o $@

.PHONY: clean all

all: $(OBJS)

clean:
	$(RM) -r $(BUILD_DIR)
