OBJ_DIR=obj
SRCS=$(wildcard *.lisp)
OBJS=$(SRCS:.lisp=.fas) $(SRCS:.lisp=.lib)
OBJS:=$(addprefix $(OBJ_DIR)/, $(OBJS))

.PHONY: all clean

all: $(OBJS)

$(OBJ_DIR)/%.lib $(OBJ_DIR)/%.fas : %.lisp | $(OBJ_DIR)
	clisp -q -c $<
	mv $(<:.lisp=.fas) $(<:.lisp=.lib) $(OBJ_DIR)/

$(OBJ_DIR) :
	@-mkdir -pv $(OBJ_DIR)

clean:
	@-rm -frv $(OBJ_DIR) 2>/dev/null
