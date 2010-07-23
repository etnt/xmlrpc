.PHONY: all clean

all:
	test -e ebin || mkdir ebin
	cd src && erl -make

clean:
	rm -f ebin/*.beam
