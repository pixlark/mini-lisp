NAME=lisp
SRC=lisp.c
HDR=
OPTIONS=-g
WARNINGS=-Werror -Wall -Wpedantic -Wextra -Wno-unused-variable

all: $(NAME)

$(NAME): $(SRC) $(HDR)
	g++ $(OPTIONS) $(WARNINGS) $(SRC) -o $@

.PHONY: all clean

clean:
	rm -f $(NAME)
