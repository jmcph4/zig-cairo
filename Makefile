SRC_DIR=src

.PHONY: all

all: build test lint

build: $(SRC_DIR)/*.zig
	zig build

test: $(SRC_DIR)/*.zig
	zig test src/decode.zig --mod types::src/types.zig --deps types

lint: $(SRC_DIR)/*.zig
	zig fmt $^

.PHONY: clean

clean:
	rm -rf zig-out zig-cache

