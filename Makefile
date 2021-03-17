##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile to build project
##
CFLAGS = -W -Wall -Wextras 
CC = ghc
EXEC = wolfram
OBJ = $(SRC:.hs=.o)
HI = $(SRC:.hs=.hi)
SRC = wolfram.hs

all: $(EXEC)

$(EXEC):
	$(CC) $(SRC) --make -o $(EXEC)

clean:
	rm -f $(OBJ)
	rm -f $(HI)

fclean: clean
	rm -f $(EXEC)

re:		fclean all

.PHONY: 	all clean fclean re
