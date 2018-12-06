.DEFAULT_GOAL := Main

TARGETS := Main
OBJS := Main.ibc

%: %.idr
	idris $< -o $@

.PHONY: clean
clean:
	rm -rf ${TARGETS} ${OBJS}
