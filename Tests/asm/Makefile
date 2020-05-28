.PHONY: build32 build64 clean

SRCDIR=src
BUILDDIR=build

SOURCES=$(notdir $(wildcard $(SRCDIR)/*.S))
TESTS=$(basename $(SOURCES))
ELF=$(addprefix build/,$(TESTS))
ELF32=$(addsuffix 32,$(ELF))
ELF64=$(addsuffix 64,$(ELF))

RISCVCC64=riscv64-unknown-elf-gcc -march=rv64ima -mabi=lp64  -static -nostdlib -nostartfiles -mcmodel=medany
RISCVCC32=riscv32-unknown-elf-gcc -march=rv32ima -mabi=ilp32 -static -nostdlib -nostartfiles -mcmodel=medany

build32: $(ELF32)

build64: $(ELF64)

init32.o: init.S
	$(RISCVCC32) -c init.S -o init32.o

init64.o: init.S
	$(RISCVCC64) -c init.S -o init64.o

$(BUILDDIR)/%32: $(SRCDIR)/%.S init32.o mmio.ld
	mkdir -p $(BUILDDIR)
	$(RISCVCC32) -c $(SRCDIR)/$*.S -o intermediate32.o
	$(RISCVCC32) -o $(BUILDDIR)/$*32 -Tmmio.ld intermediate32.o init32.o
	rm intermediate32.o

$(BUILDDIR)/%64: $(SRCDIR)/%.S init64.o mmio.ld
	mkdir -p $(BUILDDIR)
	$(RISCVCC64) -c $(SRCDIR)/$*.S -o intermediate64.o -D x64
	$(RISCVCC64) -o $(BUILDDIR)/$*64 -Tmmio.ld intermediate64.o init64.o
	rm intermediate64.o

clean:
	rm -f intermediate32.o init32.o
	rm -f intermediate64.o init64.o
	rm -rf build
