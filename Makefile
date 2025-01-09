##
## EPITECH PROJECT, 2025
## DoOp
## File description:
## Makefile
##

CC := ghc

SRC = DoOp.hs
OBJ = $(SRC:.hs=.o) $(SRC:.hs=.hi)
NAME = doop

all: $(NAME)

$(NAME):
	$(CC) $(SRC) -o $(NAME)

clean:
	@ rm -f $(OBJ)

fclean: clean
	@ rm -f $(NAME)

re: fclean all

.PHONY: $(NAME) clean fclean re
