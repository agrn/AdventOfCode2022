SOLVERS = $(patsubst %/solve.ml,%/a.out,$(wildcard */solve.ml))
TMP_FILES = $(patsubst %/solve.ml,%/solve.cmi,$(wildcard */solve.ml)) \
	$(patsubst %/solve.ml,%/solve.cmo,$(wildcard */solve.ml))

all: $(SOLVERS)

%/a.out: %/solve.ml
	@echo "	OCAMLC	$<"
	@ocamlc $< -o $@
	@echo "	TEST	$@"
	@$@ $*/test
	@echo "	RUN	$@"
	@$@ $*/input

clean:
	@echo "	RM"
	@rm -f $(SOLVERS) $(TMP_FILES)

.PHONY: all clean
