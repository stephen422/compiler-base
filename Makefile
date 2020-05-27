CFLAGS += -g -std=c11 -fsanitize=address,leak,undefined -Wall -Wextra -Wno-unused-function
# CFLAGS += -g -std=c11 -Wall -Wextra -Wno-unused-function
PROG := qc
SRCS := main.c sema.c parser.c lexer.c sds.c
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -fuse-ld=lld -o $(PROG) $(OBJS)

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -MMD -MP -c -o $@ $<

.PHONY: clean
clean:
	rm -f $(OBJS) $(DEPS) $(PROG)

-include $(DEPS)
