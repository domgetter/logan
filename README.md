# LogAn

Logic Analyzer for the Everdrive N8 Pro (NES and Famicom) OSv2.15 or greater

## Install

1. Get logan.nes and logan.rbf from the most recent Release
2. Copy these two files together to their own folder on your SD card
3. Run the N8 Pro, navigate to the folder containing logan.nes, and run the rom!
4. Press A to do a reading of the CPU clock (M2) and the PPU read pin (PPU /RD)
5. Try Select+A and Select+B to change which pin gets read in the top slot
6. Try Start+A and Start+B to change which pin gets read in the bottom slot
7. For hardware testers: Once you have written down the alignment, press Down+Start to exit the rom and preserve the current CPU/PPU alignment.  Note that this alignment is the "CPU PPU Write Alignment" and is measured in nanoseconds. (It is the time between PPU /WR going low, and the next time M2 goes high) You can see it in action if you go to Scanline 243, Dot 0, showing pins 00 and 28 (M2 and PPU /WR).

## Controls

* Controller 1:
  * A - start a reading of the pins
  * Select + A/B - change pin 0
  * Start + A/B - change pin 1
  * Left/Right - change starting dot to record voltages
  * Up/Down - change starting scanline to record voltages

* Controller 2:
  * Up/Down - scroll through output one line at a time
  * Right/Left - change polling rate, aka zoom level

* Alt controls:
  * Controller 1 SNES:
    * left shoulder: zoom in
    * right shoulder: zoom out
    * SNES A - scroll down through data
    * SNES X - scroll up through data

## Pin designations

* **00** M2
* **01** CPU R/W
* **02** CPU A0
* **03** CPU A1
* **04** CPU A2
* **05** CPU A3
* **06** CPU A4
* **07** CPU A5
* **08** CPU A6
* **09** CPU A7
* **10** CPU A8
* **11** CPU A9
* **12** CPU A10
* **13** CPU A11
* **14** CPU A12
* **15** CPU A13
* **16** CPU A14
* **17** CPU A15
* **18** CPU D0
* **19** CPU D1
* **20** CPU D2
* **21** CPU D3
* **22** CPU D4
* **23** CPU D5
* **24** CPU D6
* **25** CPU D7
* **26** /IRQ
* **27** PPU /RD
* **28** PPU /WR
* **29** PPU A0
* **30** PPU A1
* **31** PPU A2
* **32** PPU A3
* **33** PPU A4
* **34** PPU A5
* **35** PPU A6
* **36** PPU A7
* **37** PPU A8
* **38** PPU A9
* **39** PPU A10
* **40** PPU A11
* **41** PPU A12
* **42** PPU A13
* **43** PPU /A13
* **44** PPU D0
* **45** PPU D1
* **46** PPU D2
* **47** PPU D3
* **48** PPU D4
* **49** PPU D5
* **50** PPU D6
* **51** PPU D7
* **52** CIRAM /CE
* **53** CIRAM A10
* **54** PPU address currently in nametable region
* **55** PPU address currently in attribute region
* **56** Mapper is asserting on CPU data bus

## Changelog

### 0.4

* Public alpha!
* Experimental Ruler cursor mode: Press Select+Right to turn it on or off
* Repeat pin number display next to pin name to make it more clear

### 0.3

* Readable UI for pin names, denoting scanline and dot
* Show the current CPU/PPU alignment in PPU /WR nanosecond timing
* Show current Logan version on screen for better reporting
* Automatically recapture on zoom in/out
* Automatically change scanline when dot overflows
  * when dot goes from 340 to 0, scanline will increase, and decrease on the opposite

### 0.2

* scanline support
* repeat input for pin select
  * change start/select to be modifiers for A/B
    * when holding select, A increases pin 0, B decreases
    * when holding start, A increases pin 0, B decreases
* duplicate scale and paging features to 2nd controller
  * up and down scroll lines of output
  * right and left change scaling
* color voltage output with emphasis to maintain grayscale clarity (thank you Zeta)
  * coloring ignored when updating