TARGET = run_abf_io
SOURCES = src/*.ad?

# rule to link the program
abf_io: $(SOURCES)
	gprbuild -P abf_io.gpr
