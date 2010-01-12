
main: main.o sharpen.o color.o read.o 
	g++ -I $OPENCL_HOME -lOpenCL main.o sharpen.o color.o read.o -o filter 

clean: 
	rm -r main.o sharpen.o color.o read.o 