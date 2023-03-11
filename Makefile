#TOP = ./

#CFLAGS += -Wall -Wextra --make -o

#SRC = Main.hs \

NAME = glados

all: $(NAME)

$(NAME):
	stack install --local-bin-path .
	mv glados-exe glados

clean:
	rm -f *.hi
	rm -f *.o
	stack clean

fclean: clean
	rm -f $(NAME)

re: clean fclean all
