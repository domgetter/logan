logan.nes: logan.o
	ld65 -C nes_40k.cfg -o logan.nes logan.o --dbgfile logan.dbg

logan.o: logan.asm voltages.chr voltages_solid.chr letter_ui.chr
	ca65 logan.asm -o logan.o -g

everdrive: logan.nes
	@echo "\t Copying to Everdrive N8 Pro"
	@cmd.exe /c edlink-n8 -cp logan.nes sd:/logan_dev/
	@echo ""
