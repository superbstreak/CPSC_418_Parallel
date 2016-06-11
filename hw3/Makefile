NVCC      = /cs/local/lib/pkg/cudatoolkit/bin/nvcc
CFLAGS    = -O3 
LDFLAGS   =

OBJ     = time_it.o
HDRS    = time_it.h

hw3: hw3.o $(OBJ)
	$(NVCC) $(CFLAGS) hw3.o $(OBJ) $(LDFLAGS) -o hw3

perc: perc.o $(OBJ)
	$(NVCC) $(CFLAGS) perc.o $(OBJ) $(LDFLAGS) -o perc

timing: timing.o $(OBJ)
	$(NVCC) $(CFLAGS) timing.o $(OBJ) $(LDFLAGS) -o timing

.SUFFIXES: .c .cu .o
.c.o: $(HDRS)
	$(NVCC) -c $(CFLAGS) $<
.cu.o: $(HDRS)
	$(NVCC) -c $(CFLAGS) $<
