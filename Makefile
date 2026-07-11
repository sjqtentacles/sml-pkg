# sml-pkg build
#
# The PURE core (lib/.../sml-pkg) is exercised by the dual-compiler,
# byte-identical golden suite:
#   make test       build + run tests under MLton (default)
#   make test-poly  build + run tests under Poly/ML
#   make all-tests  run the suite under both compilers
#   make example    build + run the deterministic demo
#
# The IMPURE CLI driver (cli/) is a TOOL, not part of the purity guarantee:
#   make driver     build bin/sml-pkg (MLton; links the vendored sml-cli)
#   make smoke      build the driver, then run `sml-pkg resolve` here
#   make clean      remove build artifacts

MLTON      ?= mlton
BIN        := bin
LIBDIR     := lib/github.com/sjqtentacles/sml-pkg
CLIDIR     := lib/github.com/sjqtentacles/sml-cli
TEST_MLB   := test/sources.mlb
CLI_MLB    := cli/pkg.mlb
SRCS       := $(wildcard $(LIBDIR)/*.sml $(LIBDIR)/*.sig) $(wildcard test/*.sml) $(TEST_MLB) $(LIBDIR)/sources.mlb
DRIVER_SRCS := $(wildcard $(LIBDIR)/* $(CLIDIR)/* cli/*) $(CLI_MLB)

.PHONY: all test poly test-poly verify-identical all-tests example driver smoke clean example-poly

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

all-tests: test test-poly verify-identical

# Demos are top-level scripts; run them under Poly/ML via use-loading.
example-poly:
	sh tools/polybuild -r examples/sources.mlb

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

driver: $(BIN)/sml-pkg

$(BIN)/sml-pkg: $(DRIVER_SRCS) | $(BIN)
	$(MLTON) -output $@ $(CLI_MLB)

smoke: driver
	./$(BIN)/sml-pkg resolve

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)

# The dual-compiler contract: both suites must print byte-identical output.
# Recursive make -s captures the raw suite stdout regardless of poly strategy.
verify-identical:
	$(MAKE) -s test > $(BIN)/out-mlton.txt
	$(MAKE) -s test-poly > $(BIN)/out-poly.txt
	diff $(BIN)/out-mlton.txt $(BIN)/out-poly.txt
	@echo "byte-identical: OK"
