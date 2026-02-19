.PHONY: build test test-verbose diag-oot run-oot run-sm64 run-mm clean

CARGO = cargo
RELEASE = --release
BINARY = target/release/n64-frontend

# ROMs
OOT_ROM = test_roms/zelda-oot.z64
SM64_ROM = test_roms/sm64.z64
MM_ROM = test_roms/zelda-mm.z64
TEST_ROMS = test_roms/n64-tests/roms-simpleboot/*.z64

build:
	$(CARGO) build $(RELEASE)

# Run all 26 simpleboot CPU instruction tests
test: build
	@pass=0; fail=0; \
	for rom in $(TEST_ROMS); do \
		name=$$(basename "$$rom"); \
		result=$$($(BINARY) --test "$$rom" 2>/dev/null); \
		if echo "$$result" | grep -q "PASS"; then \
			pass=$$((pass+1)); \
		else \
			echo "FAIL: $$name"; \
			fail=$$((fail+1)); \
		fi; \
	done; \
	echo "Results: $$pass passed, $$fail failed"; \
	[ $$fail -eq 0 ]

# Same but with per-test output
test-verbose: build
	@for rom in $(TEST_ROMS); do \
		name=$$(basename "$$rom"); \
		result=$$($(BINARY) --test "$$rom" 2>/dev/null); \
		echo "$$name: $$result"; \
	done

# Run OoT boot diagnostic (2000M steps)
diag-oot: build
	$(BINARY) --diag $(OOT_ROM)

# Run games in graphical window
run-oot: build
	$(BINARY) $(OOT_ROM)

run-sm64: build
	$(BINARY) $(SM64_ROM)

run-mm: build
	$(BINARY) $(MM_ROM)

clean:
	$(CARGO) clean
