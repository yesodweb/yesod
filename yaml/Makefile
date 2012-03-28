libhelper.so : helper.o
	gcc -shared -Wl,-soname,libhelper.so -o libhelper.so helper.o
helper.o : c/helper.c
	gcc -c -fPIC c/helper.c -o helper.o
