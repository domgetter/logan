all: logan.nes edn8/000/output_files/logan.rbf

logan.nes: logan.o
	ld65 -C nes_40k.cfg -o logan.nes logan.o --dbgfile logan.dbg

logan.o: logan.asm voltages.chr voltages_solid.chr letter_ui.chr
	ca65 logan.asm -o logan.o -g

everdrive: all
	@cmd.exe /c edlink-n8 -mkdir sd:/logan_dev
	@echo "\t Copying to Everdrive N8 Pro"
	@cmd.exe /c edlink-n8 -cp logan.nes sd:/logan_dev/
	@cmd.exe /c edlink-n8 -cp edn8/000/output_files/logan.rbf sd:/logan_dev/
	@echo ""

# Recursive make to build the mapper
.PHONY: edn8/000/output_files/logan.rbf
edn8/000/output_files/logan.rbf:
	$(MAKE) -C edn8/000
