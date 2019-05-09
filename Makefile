# := indicar que no cambie aun si después se modifica
BINDIR := bin/
SRCDIR := src/
#OUTPUTDIR := out/
EXECUTABLE := methods.out

CC = gfortran

# Define file extensions
.SUFFIXES:
.SUFFIXES: .f95 .o .mod

#all f95 files
#SRC	= $(wildcard $(SRCDIR)*.f95)
#_OBJS	= $(patsubst $(SRCDIR)%.f95, %.o, $(SRC))
#OBJS	= $(addprefix $(BINDIR), $(_OBJS))

#en orden de compilacion
OBJS := $(BINDIR)modulo_f.o $(BINDIR)BisectionMethod.o $(BINDIR)FalsePositionMethod.o $(BINDIR)NewtonMethod.o \
	$(BINDIR)SecanteMethod.o $(BINDIR)GaussianEliminationMethod.o $(BINDIR)LU_DecompositionMethod.o $(BINDIR)GaussSeidelMethod.o \
	$(BINDIR)PowerSeriesMethod.o $(BINDIR)LagrangeMethod.o $(BINDIR)NewtonDividedDifference.o \
	$(BINDIR)RootFindingMethods.o $(BINDIR)SystemOfLinearEquationsSolver.o $(BINDIR)InterpolationMethods.o \
	$(BINDIR)PRMethod.o $(BINDIR)Main.o

# Build rules
all: banner | program #esto se ejecuta solo con decir make

# @ para que no se muestre en la salida esatandar
banner: $(BINDIR)
	@echo $$(echo `date +'%H:%M:%S'`) Looking for files that need to be compiled ...

$(BINDIR):
	@mkdir -p $(BINDIR)
#@mkdir -p $(OUTPUTDIR)

program: $(OBJS)
	@echo $$(echo `date +'%H:%M:%S'`) Completing compilation ...
	@echo $$(echo `date +'%H:%M:%S'`) Creating executable ...
	@$(CC) -o $(EXECUTABLE) $(BINDIR)*o
#@rm $(BINDIR)*.mod
	@echo $$(echo `date +'%H:%M:%S'`) Done with out errors!
	@echo Try ./$(EXECUTABLE) to start and enjoy the program
	@echo For details about inputs and outputs look at README.md

$(BINDIR)%.o: $(SRCDIR)%.f95
	@$(CC) -c $^ -o $@ -J$(BINDIR)

$(BINDIR)%.o: $(SRCDIR)root_finding/%.f95
	@$(CC) -c $^ -o $@ -J$(BINDIR)

$(BINDIR)%.o: $(SRCDIR)system_linear_equations/%.f95
	@$(CC) -c $^ -o $@ -J$(BINDIR)

$(BINDIR)%.o: $(SRCDIR)interpolation/%.f95
	@$(CC) -c $^ -o $@ -J$(BINDIR)

$(BINDIR)%.o: $(SRCDIR)regression/%.f95
	@$(CC) -c $^ -o $@ -J$(BINDIR)

clean:
	@echo Deleting binary files...
	@rm -rf $(BINDIR)
	@rm -f $(EXECUTABLE)
	@echo WARNING! deleting outputs ...
	@rm *txt
