CFLAGS = `pkg-config guile-2.2 --cflags`
LIBS   = `pkg-config guile-2.2 --libs`

O_FILES = image-type.o myguile.o

all: myguile

myguile: $(O_FILES)
	$(CC) $(O_FILES) $(LIBS) -o myguile

clean:
	-rm -rf myguile $(O_FILES)
