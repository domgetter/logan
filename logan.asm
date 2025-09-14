.segment "HEADER"
  ;; "NES"
  .byte $4E,$45,$53,$1A

  ;; PRG 16k count low byte
  .byte $02

  ;; CHR 8k count low byte
  .byte $03

  ;; mapper low nibble + flags
  .byte $03

  ;; mapper high nibble + flags
  .byte $08

  ;; submapper + mapper high byte
  .byte $00

  ;; CHR/PRG high byte nibbles
  .byte $00

  ;; EEPROM + PRG RAM size
  .byte $00

  ;; CHR RAM size
  .byte $00

  ;; CPU/PPU timing
  .byte $00

  ;; hardware type
  .byte $00

  ;; other ROMs
  .byte $00

  ;; default expansion device
  .byte $00

.segment "ZEROPAGE"
  general_functions: .res 32
.segment "OAMRAM"
.segment "INTERNALRAM"
  PPU_CTRL_2000 := $2000
  PPU_MASK_2001 := $2001
  PPU_STATUS_2002 := $2002
  PPU_ADDR_2006 := $2006
  PPU_DATA_2007 := $2007

  VOLTAGES_PIN0 := $5000
  VOLTAGES := $5001
  VOLTAGES_INDEX_RESET := $5002
  VOLTAGES_COUNTER_RESET := $5003
  VOLTAGES_PIN1 := $5004
  VOLTAGES_POLLING_FREQUENCY := $5005
  VOLTAGES_DISPLAY_ROW := $5007
  VOLTAGES_POLLING_FREQUENCY_HIGH := $5008
  VOLTAGES_POLLING_FREQUENCY_HIGHER := $5009

  POLLING_START_X_HI := $500A
  POLLING_START_X_LO := $500B
  POLLING_START_Y_HI := $500C
  POLLING_START_Y_LO := $500D

  POLLING_DATA_READY := $500E

  MAPPER_CURRENT_SCANLINE_BUFFER := $5010
  MAPPER_CURRENT_SCANLINE_HI := $5011
  MAPPER_CURRENT_SCANLINE_LO := $5012

  RulerCursor := 1

.macro mov dest, src
  LDA src
  STA dest
.endmacro

;; Add two 16-bit values and store the result into src
;; The second value can be an address or a constant,
;;   but the first value MUST be an address.
;; ex.
;;   sta16 $00, #$0152
;;   adc16 $00, #$00A0
;; $00/$01 now contain #$01F2
;;   sta16 $00, #$FFFE
;;   adc16 $00, #$0002
;; $00/$01 now contain #$0001 and the carry will be set
.macro adc16 src, summand
  .if (.match (.left (1, {src}), #))
    .error "Cannot store into literal"
  .else
    ;; If there is no summand, presume that the accumulator is to be added
    .ifblank summand
      CLC
      ADC src
      STA src
      LDA #$00
      ADC src + 1
      STA src + 1
    .else
      .if (.match (.left (1, {summand}), #))
        ;; If the summand is a constant
        .if (.xmatch (.right (1, {src}), Y))
          ;; example:
          ;;   LDY #AreaActor::y_coord
          ;;   adc16 {(actor_ptr), Y}, #$0002
          CLC
          LDA src
          ADC #<(.right (.tcount ({summand}) - 1, {summand}))
          STA src
          INY
          LDA src
          ADC #>(.right (.tcount ({summand}) - 1, {summand}))
          STA src
        .else
          ;; example:
          ;;   adc16 camera_x, #$0010
          CLC
          LDA src
          ADC #<(.right (.tcount ({summand}) - 1, {summand}))
          STA src
          LDA src + 1
          ADC #>(.right (.tcount ({summand}) - 1, {summand}))
          STA src + 1
        .endif
      .else
        .if (.xmatch (.right (1, {src}), Y))
          ;; example:
          ;;   LDY #AreaActor::x_coord
          ;;   adc16 {(actor_ptr), Y}, camera_x
          ;; Why would you add the camera to the actor's position? Who knows, it's just an example
          CLC
          LDA src
          ADC summand
          STA src
          INY
          LDA src
          ADC summand + 1
          STA src
        .elseif (.xmatch (.right (1, {summand}), Y))
          ;; example:
          ;;   LDY #AreaActor::x_coord
          ;;   adc16 camera_x, {(actor_ptr), Y}
          CLC
          LDA src
          ADC summand
          STA src
          INY
          LDA src + 1
          ADC summand
          STA src + 1
        .else
          ;; example:
          ;;   adc16 camera_x, camera_x_offset
          CLC
          LDA src
          ADC summand
          STA src
          LDA src + 1
          ADC summand + 1
          STA src + 1
        .endif
      .endif
    .endif
  .endif
.endmacro

;; Compares two 16-bit values.  Either value may be
;;   an adress or a constant.  The carry flag
;;   will be set if the first value is >= the
;;   second value, and the zero flag will be set
;;   if the values are equal.  Makes no guarantees
;;   for the Negative flag, so don't go BMIing or BPLing.
;; ex.
;;   sta16 $00, #$1234
;;   cmp16 $00, #$2000
;; carry will be clear, and zero will be clear
;;   sta16 $00, #$1234
;;   cmp16 $00, #$1000
;; carry will be SET, and zero will be clear
;;   sta16 $00, #$1234
;;   cmp16 $00, #$1234
;; carry will be SET, and zero will be SET
;;
;;          C Z
;;  A < B   0 0
;;  A > B   1 0
;;  A = B   1 1
.macro cmp16 src, target
  ;; If the source is a literal, be sure to break up bytes manually
  .if (.match (.left (1, {src}), #))
    ;; Source is a literal
    ;; If target is a literal, be sure to break up bytes manually
    .if (.match (.left (1, {target}), #))
      ;; Source and target are both literals
      .local end_cmp16_both_constant
      LDA #>(.right (.tcount ({src})-1, {src}))
      CMP #>(.right (.tcount ({target})-1, {target}))
      BNE end_cmp16_both_constant
        LDA #<(.right (.tcount ({src})-1, {src}))
        CMP #<(.right (.tcount ({target})-1, {target}))
      end_cmp16_both_constant:
    .elseif(.xmatch (.right (1, {target}), Y))
      .local end_cmp16_src_constant_indirect
      INY
      LDA #>(.right (.tcount ({src})-1, {src}))
      CMP target
      BNE end_cmp16_src_constant_indirect
        DEY
        LDA #<(.right (.tcount ({src})-1, {src}))
        CMP target
      end_cmp16_src_constant_indirect:
    .else
      .local end_cmp16_src_constant
      LDA #>(.right (.tcount ({src})-1, {src}))
      CMP target + 1
      BNE end_cmp16_src_constant
        LDA #<(.right (.tcount ({src})-1, {src}))
        CMP target
      end_cmp16_src_constant:
    .endif
  .else
    ;; Source is not a literal
    ;; If target is a literal, be sure to break up bytes manually
    .if (.match (.left (1, {target}), #))
      ;; Target is a literal, but source is not

      .if(.xmatch (.right (1, {src}), Y))
        .local end_cmp16_target_literal_indirect
        INY
        LDA src
        CMP #>(.right (.tcount ({target})-1, {target}))
        BNE end_cmp16_target_literal_indirect
          DEY
          LDA src
          CMP #<(.right (.tcount ({target})-1, {target}))
        end_cmp16_target_literal_indirect:
      .else
        .local end_cmp16_target_literal
        LDA src + 1
        CMP #>(.right (.tcount ({target})-1, {target}))
        BNE end_cmp16_target_literal
          LDA src
          CMP #<(.right (.tcount ({target})-1, {target}))
        end_cmp16_target_literal:
      .endif
    .else
      ;; Neither source nor target are literals

      .if(.xmatch (.right (1, {src}), Y))
        .local end_cmp16_src_indirect
        INY
        LDA src
        CMP target + 1
        BNE end_cmp16_src_indirect
          DEY
          LDA src
          CMP target
        end_cmp16_src_indirect:
      .elseif (.xmatch (.right (1, {target}), Y))
        .local end_cmp16_target_indirect
        INY
        LDA src + 1
        CMP target
        BNE end_cmp16_target_indirect
          DEY
          LDA src
          CMP target
        end_cmp16_target_indirect:
      .else
        .local end_cmp16_normal
        LDA src + 1
        CMP target + 1
        BNE end_cmp16_normal
          LDA src
          CMP target
        end_cmp16_normal:
      .endif
    .endif
  .endif
.endmacro

;; Subtract two 16-bit values and store the result into src
;; The second value can be an address or a constant,
;;   but the first value MUST be an address.
;; ex.
;;   sta16 $00, #$0112
;;   sbc16 $00, #$0080
;; $00/$01 now contain #$0092
;; Allowed polymorphism:
;;  sbc16 mem1, mem2
;;  sbc16 mem, #val
;;  sbc16 {(ptr),Y}, mem
;;  sbc16 {(ptr),Y}, #val
;;  sbc16 mem, {(ptr), Y}
.macro sbc16 src, diff
  .if (.match (.left (1, {src}), #))
    .error "Cannot store into literal"
  .else
    .if (.match (.left (1, {diff}), #))
      .if(.match (.right (1, {src}), Y))
        SEC
        LDA src
        SBC #<(.right (.tcount ({diff})-1, {diff}))
        STA src
        INY
        LDA src
        SBC #>(.right (.tcount ({diff})-1, {diff}))
        STA src
      .else
        SEC
        LDA src
        SBC #<(.right (.tcount ({diff})-1, {diff}))
        STA src
        LDA src + 1
        SBC #>(.right (.tcount ({diff})-1, {diff}))
        STA src + 1
      .endif
    .else
      ;.error "++++++++++matches"
      ;.error .string(.match (.left(1,{.right (1, {diff})}), Y))
      ;.error " "
      ;.if(.match (.left(1,{.right (1, {diff})}), Y))
      .if(.match (.right (1, {diff}), Y))
        .if(.match (.right (1, {src}), Y))
          SEC
          LDA src
          SBC diff
          STA src
          INY
          LDA src
          SBC diff
          STA src
        .else
          SEC
          LDA src
          SBC diff
          STA src
          LDA src + 1
          INY
          SBC diff
          STA src + 1
        .endif
      .else
        .if(.match (.right (1, {src}), Y))
          SEC
          LDA src
          SBC diff
          STA src
          INY
          LDA src
          SBC diff + 1
          STA src
        .else
          SEC
          LDA src
          SBC diff
          STA src
          LDA src + 1
          SBC diff + 1
          STA src + 1
        .endif
      .endif
    .endif
  .endif
.endmacro


  .struct Controller
    a_button .byte
    b_button .byte
    select .byte
    start .byte
    up .byte
    down .byte
    left .byte
    right .byte
    snes_a .byte
    snes_x .byte
    snes_l .byte
    snes_r .byte
  .endstruct

  ppu_ctrl_buffer: .res 1
  ppu_mask_buffer: .res 1
  controller: .tag Controller
  controller_last_frame: .tag Controller
  controller_2: .tag Controller
  controller_2_last_frame: .tag Controller
  vblank_happened: .res 1
  nt_buffer_chunk: .res 1
  pin0_target: .res 1
  pin1_target: .res 1
  skip_sprite_0_check: .res 1
  nt_buffer: .res 256
  polling_freq: .res 3
  display_row: .res 1
  update_display_row: .res 1
  stuff_loaded: .res 1
  ; 16bit target dot for polling voltage data.  little endian
  target_dot: .res 2

  target_dot_increase_repeat_cooldown: .res 1
  target_dot_decrease_repeat_cooldown: .res 1

  target_scanline: .res 2
  target_scanline_for_sprite: .res 2

  target_scanline_increase_repeat_cooldown: .res 1
  target_scanline_decrease_repeat_cooldown: .res 1

  holding_start: .res 1
  holding_select: .res 1

  pin0_target_increase_repeat_cooldown: .res 1
  pin0_target_decrease_repeat_cooldown: .res 1
  pin1_target_increase_repeat_cooldown: .res 1
  pin1_target_decrease_repeat_cooldown: .res 1

  snes_controller_connected: .res 1

  current_scanline: .res 2

  update_pin_0_text: .res 1

  alignment_sum: .res 3
  
  alignment_sum_fraction_temp: .res 2

  tens_place: .res 1
  ones_place: .res 1
  tenths_place: .res 1
  hundredths_place: .res 1
  thousandths_place: .res 1

  zoom_change_r: .res 1
  zoom_change_l: .res 1

  chr_bank: .res 1
  even_frame: .res 1
  cursor_mode: .res 1

.segment "STARTUP"

scanline_text:
  .asciiz "SCANLINE: "
dot_text:
  .asciiz "     DOT: "
alignment_text:
  .asciiz "ALIGNMENT"
logan_version_text:
  .asciiz "LOGAN v0.4@"

DetectSNESController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016

  ;; ignore first 12 reads
  .repeat 12
    LDA $4016
  .endrepeat

  ;; if next read is 0, we have snes, if 1, nes
  LDA $4016
  AND #$01
  BEQ :+
    LDA #$00
    STA snes_controller_connected
    JMP :++
  :
    LDA #$01
    STA snes_controller_connected
  :

  RTS

OAMDMA:
  ;; copy sprites from $0200-$02FF
  mov $2003, #$00
  mov $4014, #$02
  RTS

InitializePPURAM:

  @ClearNametables:
    LDA $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006

    LDA #$00
    LDX #$00
    LDY #$00
  :
    STA $2007
    INX
    BNE :-
    INY
    CPY #$10
    BNE :-

  ;; Wait for the next vblank before clearing the palettes
  ;; So that we don't see a weird rainbow across the screen
  JSR WaitForNextVBlank

  @ClearPalettes:
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDA #$0F
    LDX #$00
  :
    STA $2007
    INX
    CPX #$20
    BNE :-

  RTS

WaitForNextVBlank:

  BIT $2002
  BPL WaitForNextVBlank
  RTS

DisableVblankInterrupt:
  ; enable VBlank interrupt
  LDA #%00000000
  STA $2000
  RTS

DisableRendering:
  ; disable all rendering
  LDA #%00000000
  STA $2001
  RTS

DisableAPUInterrupts:

  ; disable APU frame counter interrupt
  LDX #DISABLE_APU_FC_INT
  STX $4017

  ; disable DMC interrupts
  LDA #$00
  STA $4010

  RTS

;; Collect controller input from player
ControllerInput:

  LDA $FF

  .repeat 12, i
    mov controller_last_frame + i, controller + i
  .endrepeat
  
  .repeat 8, i
    mov controller_2_last_frame + i, controller_2 + i
  .endrepeat

  ;; raise and lower voltage of controller line to latch
  mov $4016, #$01
  mov $4016, #$00

  LDA $4016
  AND #$01
  STA controller + Controller::a_button
  LDA $4016
  AND #$01
  STA controller + Controller::b_button
  LDA $4016
  AND #$01
  STA controller + Controller::select
  LDA $4016
  AND #$01
  STA controller + Controller::start
  LDA $4016
  AND #$01
  STA controller + Controller::up
  LDA $4016
  AND #$01
  STA controller + Controller::down
  LDA $4016
  AND #$01
  STA controller + Controller::left
  LDA $4016
  AND #$01
  STA controller + Controller::right
  LDA snes_controller_connected
  BEQ :+
    LDA $4016
    AND #$01
    STA controller + Controller::snes_a
    LDA $4016
    AND #$01
    STA controller + Controller::snes_x
    LDA $4016
    AND #$01
    STA controller + Controller::snes_l
    LDA $4016
    AND #$01
    STA controller + Controller::snes_r
  :

  ;; Now read controller 2
  
  LDA $4017
  AND #$01
  STA controller_2 + Controller::a_button
  LDA $4017
  AND #$01
  STA controller_2 + Controller::b_button
  LDA $4017
  AND #$01
  STA controller_2 + Controller::select
  LDA $4017
  AND #$01
  STA controller_2 + Controller::start
  LDA $4017
  AND #$01
  STA controller_2 + Controller::up
  LDA $4017
  AND #$01
  STA controller_2 + Controller::down
  LDA $4017
  AND #$01
  STA controller_2 + Controller::left
  LDA $4017
  AND #$01
  STA controller_2 + Controller::right

  RTS

STACK_POINTER_INIT := $FF
DISABLE_APU_FC_INT := $40

RESET:
  ; ignore interrupts during initialization
  SEI

  ; initialize stack to empty
  LDX #STACK_POINTER_INIT
  TXS
  LDX #$00

  JSR DisableVblankInterrupt
  JSR DisableRendering
  JSR DisableAPUInterrupts

  ; Done inline since we overwrite the stack
  InitializeCPURAM:

    LDX #$00
    :
    LDA #$00
    STA $0000, x
    STA $0100, x
    STA $0300, x
    STA $0400, x
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FF
    STA $0200, x
    INX
    BNE :-

  JSR InitializePPURAM

  LDA #$20
  STA $2006
  LDA #$00
  STA $2006
  LDA #$55 ;; blank tile
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    BNE :-
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    BNE :-
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    BNE :-
  LDX #$40
  :
    STA PPU_DATA_2007
    INX
    BNE :-

  ;; Set attributes for voltage table
  
  LDA #$23
  STA PPU_ADDR_2006
  LDA #$C0
  STA PPU_ADDR_2006
  LDX #$00
  :
    LDA display_attributes, X
    STA PPU_DATA_2007
    INX
    CPX #$40
    BNE :-

  ;; set attributes to all black for other nametable
  
  LDA #$27
  STA PPU_ADDR_2006
  LDA #$C0
  STA PPU_ADDR_2006
  LDA #$FF
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    CPX #$40
    BNE :-

  ;; set attributes for places for readable text
  LDA #$23
  STA PPU_ADDR_2006
  LDA #$C0
  STA PPU_ADDR_2006
  LDA #%01010101
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    CPX #$10
    BNE :-

  ;; set blank tile for places for readable text
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$00
  STA PPU_ADDR_2006
  LDA #$00
  LDX #$00
  :
    STA PPU_DATA_2007
    INX
    BNE :-

  ;; Write "scanline" to the place in the nametable that we want
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$42
  STA PPU_ADDR_2006
  LDX #$00
  scanline_rendering_loop:
    LDA scanline_text, X
    BEQ done_rendering_scanline_text
    STA PPU_DATA_2007
    INX
    JMP scanline_rendering_loop
  done_rendering_scanline_text:
  ;; Write "dot" to the place in the nametable that we want
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$62
  STA PPU_ADDR_2006
  LDX #$00
  dot_rendering_loop:
    LDA dot_text, X
    BEQ done_rendering_dot_text
    STA PPU_DATA_2007
    INX
    JMP dot_rendering_loop
  done_rendering_dot_text:
  ;; Write "alignment" to the place in the nametable that we want
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$51
  STA PPU_ADDR_2006
  LDX #$00
  alignment_rendering_loop:
    LDA alignment_text, X
    BEQ done_rendering_alignment_text
    STA PPU_DATA_2007
    INX
    JMP alignment_rendering_loop
  done_rendering_alignment_text:
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$C2
  STA PPU_ADDR_2006
  LDX #$00
  logan_rendering_loop:
    LDA logan_version_text, X
    BEQ done_rendering_logan_text
    STA PPU_DATA_2007
    INX
    JMP logan_rendering_loop
  done_rendering_logan_text:

  ;; Force chr bank to 2 for solid voltage render
  LDA #$00
  STA chr_bank

  LDA chr_bank
  STA $5020

  ;; For some reason, the C of SCANLINE is blank?  Just write it again here...
  ;; TODO: figure out what the heck is going on. Mesen doesn't report any extra writes to that location...
  LDA $2002
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$43
  STA PPU_ADDR_2006
  LDA #$43
  STA PPU_DATA_2007
  

  LDA #$F0
  STA $2005
  LDA #$00
  STA $2005
; turn PPU rendering back on
  LDA $2002
  LDA #%00011110			; no intensify(black bg), enable sprites
  STA $2001
  STA ppu_mask_buffer

; tell PPU to use Pattern Table 1 tiles for sprites, and Pattern Table 0 for bg
  LDA #%10001001			; enable NMI, sprites 8x8
  STA ppu_ctrl_buffer
  STA $2000

  CLI

  LDA #$00
  STA pin0_target
  LDA #$1B
  STA pin1_target

  LDA #$01
  STA skip_sprite_0_check

  JSR DetectSNESController

  JMP Main

Main:

  LDA controller + Controller::select
  BEQ :+
    LDA #$01
    STA holding_select
    JMP :++
  :
    LDA #$00
    STA holding_select
  :

  LDA holding_select
  BEQ :+
    LDA controller_last_frame + Controller::right
    CMP controller + Controller::right
    BCS :+
      LDA cursor_mode
      EOR #$01
      STA cursor_mode
  :

  LDA controller + Controller::start
  BEQ :+
    LDA #$01
    STA holding_start
    JMP :++
  :
    LDA #$00
    STA holding_start
  :

  PIN_REPEAT_COOLDOWN := $18

  LDA holding_select
  BEQ done_checking_select1
    LDA controller_last_frame + Controller::a_button
    CMP controller + Controller::a_button
    BCS done_checking_select_increase1
      ;; Increase pin0 by one, and start repeat cooldown
      INC pin0_target
      LDA pin0_target
      CMP #57
      BNE :+
        LDA #$00
        STA pin0_target
      :

      LDA #PIN_REPEAT_COOLDOWN
      STA pin0_target_increase_repeat_cooldown
    done_checking_select_increase1:
  done_checking_select1:

  LDA holding_select
  BEQ done_checking_select2
    LDA controller_last_frame + Controller::b_button
    CMP controller + Controller::b_button
    BCS done_checking_select_increase2
      ;; Decrease pin0 by one, and start repeat cooldown
      DEC pin0_target
      LDA pin0_target
      CMP #$FF
      BNE :+
        LDA #56
        STA pin0_target
      :

      LDA #PIN_REPEAT_COOLDOWN
      STA pin0_target_decrease_repeat_cooldown
    done_checking_select_increase2:
  done_checking_select2:
  

  LDA holding_start
  BEQ done_checking_start1
    LDA controller_last_frame + Controller::a_button
    CMP controller + Controller::a_button
    BCS done_checking_start_increase1
      ;; Increase pin1 by one, and start repeat cooldown
      INC pin1_target
      LDA pin1_target
      CMP #57
      BNE :+
        LDA #$00
        STA pin1_target
      :

      LDA #PIN_REPEAT_COOLDOWN
      STA pin1_target_increase_repeat_cooldown
    done_checking_start_increase1:
  done_checking_start1:

  LDA holding_start
  BEQ done_checking_start2
    LDA controller_last_frame + Controller::b_button
    CMP controller + Controller::b_button
    BCS done_checking_start_increase2
      ;; Decrease pin1 by one, and start repeat cooldown
      DEC pin1_target
      LDA pin1_target
      CMP #$FF
      BNE :+
        LDA #56
        STA pin1_target
      :

      LDA #PIN_REPEAT_COOLDOWN
      STA pin1_target_decrease_repeat_cooldown
    done_checking_start_increase2:
  done_checking_start2:

  LDA holding_select
  BEQ done_repeating_increase_pin0
    LDA controller + Controller::a_button
    BEQ done_repeating_increase_pin0
      LDA pin0_target_increase_repeat_cooldown
      BNE :++
        INC pin0_target
        LDA pin0_target
        CMP #57
        BNE :+
          LDA #$00
          STA pin0_target
        :
        LDA #$02
        STA pin0_target_increase_repeat_cooldown
        JMP done_repeating_increase_pin0
      :
      DEC pin0_target_increase_repeat_cooldown
  done_repeating_increase_pin0:
  
  LDA holding_select
  BEQ done_repeating_decrease_pin0
    LDA controller + Controller::b_button
    BEQ done_repeating_decrease_pin0
      LDA pin0_target_decrease_repeat_cooldown
      BNE :++
        DEC pin0_target
        LDA pin0_target
        CMP #$FF
        BNE :+
          LDA #56
          STA pin0_target
        :
        LDA #$02
        STA pin0_target_decrease_repeat_cooldown
        JMP done_repeating_decrease_pin0
      :
      DEC pin0_target_decrease_repeat_cooldown
  done_repeating_decrease_pin0:

  LDA holding_start
  BEQ done_repeating_increase_pin1
    LDA controller + Controller::a_button
    BEQ done_repeating_increase_pin1
      LDA pin1_target_increase_repeat_cooldown
      BNE :++
        INC pin1_target
        LDA pin1_target
        CMP #57
        BNE :+
          LDA #$00
          STA pin1_target
        :
        LDA #$02
        STA pin1_target_increase_repeat_cooldown
        JMP done_repeating_increase_pin1
      :
      DEC pin1_target_increase_repeat_cooldown
  done_repeating_increase_pin1:
  
  LDA holding_start
  BEQ done_repeating_decrease_pin1
    LDA controller + Controller::b_button
    BEQ done_repeating_decrease_pin1
      LDA pin1_target_decrease_repeat_cooldown
      BNE :++
        DEC pin1_target
        LDA pin1_target
        CMP #$FF
        BNE :+
          LDA #56
          STA pin1_target
        :
        LDA #$02
        STA pin1_target_decrease_repeat_cooldown
        JMP done_repeating_decrease_pin1
      :
      DEC pin1_target_decrease_repeat_cooldown
  done_repeating_decrease_pin1:

  LDA controller_last_frame + Controller::snes_l
  CMP controller + Controller::snes_l
  BCS :+
    LDA #$01
    STA zoom_change_l
    DEC polling_freq
    LDA polling_freq
    CMP #$FF
    BNE :++
      LDA #$00
      STA polling_freq
      JMP :++
    :
    LDA #$00
    STA zoom_change_l
  :

  LDA controller_2_last_frame + Controller::left
  CMP controller_2 + Controller::left
  BCS :+
    DEC polling_freq
    LDA polling_freq
    CMP #$FF
    BNE :+
    LDA #$00
    STA polling_freq
  :

  LDA controller_last_frame + Controller::snes_r
  CMP controller + Controller::snes_r
  BCS :+
    LDA #$01
    STA zoom_change_r
    INC polling_freq
    LDA polling_freq
    CMP #$FF
    BNE :++
      LDA #$FE
      STA polling_freq
      JMP :++
    :
    LDA #$00
    STA zoom_change_r
  :

  LDA controller_2_last_frame + Controller::right
  CMP controller_2 + Controller::right
  BCS :+
    INC polling_freq
    LDA polling_freq
    CMP #$FF
    BNE :+
    LDA #$FE
    STA polling_freq
  :

  LDA controller_last_frame + Controller::snes_a
  CMP controller + Controller::snes_a
  BCS :+
    LDA #$01
    STA update_display_row
    INC display_row
    LDA display_row
    CMP #$20
    BNE :+
    LDA #$1F
    STA display_row
    LDA #$00
    STA update_display_row
  :

  LDA controller_2_last_frame + Controller::down
  CMP controller_2 + Controller::down
  BCS :+
    LDA #$01
    STA update_display_row
    INC display_row
    LDA display_row
    CMP #$20
    BNE :+
    LDA #$1F
    STA display_row
    LDA #$00
    STA update_display_row
  :

  LDA controller_last_frame + Controller::snes_x
  CMP controller + Controller::snes_x
  BCS :+
    LDA #$01
    STA update_display_row
    DEC display_row
    LDA display_row
    CMP #$FF
    BNE :+
    LDA #$00
    STA display_row
    LDA #$00
    STA update_display_row
  :

  LDA controller_2_last_frame + Controller::up
  CMP controller_2 + Controller::up
  BCS :+
    LDA #$01
    STA update_display_row
    DEC display_row
    LDA display_row
    CMP #$FF
    BNE :+
    LDA #$00
    STA display_row
    LDA #$00
    STA update_display_row
  :

  LDA controller_last_frame + Controller::right
  CMP controller + Controller::right
  BCS :+
    ;; Increase target dot by one, and start repeat cooldown
    adc16 target_dot, #$0001

    ;; Check if target dot is >=341
    cmp16 target_dot, #341
    BCC no_reset_target_dot
      LDA #$00
      STA target_dot
      STA target_dot + 1
      adc16 target_scanline, #$01
      
      ;; Check if target scanline is >=262
      cmp16 target_scanline, #262
      BCC no_reset_target_scanline0a
        LDA #$00
        STA target_scanline
        STA target_scanline + 1
      no_reset_target_scanline0a:
    no_reset_target_dot:

    LDA #$18
    STA target_dot_increase_repeat_cooldown
  :

  LDA controller + Controller::right
  BEQ :++
    LDA target_dot_increase_repeat_cooldown
    BNE :+
      ;; It has been some time since right was first being held, so just increment the target dot
      adc16 target_dot, #$0001
      ;; Check if target dot is >=341
      cmp16 target_dot, #341
      BCC no_reset_target_dot2
        LDA #$00
        STA target_dot
        STA target_dot + 1
        
        adc16 target_scanline, #$01
        
        ;; Check if target scanline is >=262
        cmp16 target_scanline, #262
        BCC no_reset_target_scanline0b
          LDA #$00
          STA target_scanline
          STA target_scanline + 1
        no_reset_target_scanline0b:
      no_reset_target_dot2:

      JMP :++
    :
      DEC target_dot_increase_repeat_cooldown
  :
  

  LDA controller_last_frame + Controller::left
  CMP controller + Controller::left
  BCS :+
    ;; Increase target dot by one, and start repeat cooldown
    adc16 target_dot, #$FFFF

    ;; Check if target dot is <0
    cmp16 target_dot, #$FFFF
    BNE no_reset_target_dot3
      LDA #<340
      STA target_dot
      LDA #>340
      STA target_dot + 1
      
      ;; Increase target scanline by one, and start repeat cooldown
      adc16 target_scanline, #$FFFF

      ;; Check if target scanline is <0
      cmp16 target_scanline, #$FFFF
      BNE no_reset_target_scanline0c
        LDA #<261
        STA target_scanline
        LDA #>261
        STA target_scanline + 1
      no_reset_target_scanline0c:
    no_reset_target_dot3:

    LDA #$18
    STA target_dot_decrease_repeat_cooldown
  :

  LDA controller + Controller::left
  BEQ :++
    LDA target_dot_decrease_repeat_cooldown
    BNE :+
      ;; It has been some time since left was first being held, so just decrement the target dot
      adc16 target_dot, #$FFFF
      ;; Check if target dot is <0
      cmp16 target_dot, #$FFFF
      BNE no_reset_target_dot4
        LDA #<340
        STA target_dot
        LDA #>340
        STA target_dot + 1

        ;; Increase target scanline by one, and start repeat cooldown
        adc16 target_scanline, #$FFFF

        ;; Check if target scanline is <0
        cmp16 target_scanline, #$FFFF
        BNE no_reset_target_scanline0d
          LDA #<261
          STA target_scanline
          LDA #>261
          STA target_scanline + 1
        no_reset_target_scanline0d:
      no_reset_target_dot4:

      JMP :++
    :
      DEC target_dot_decrease_repeat_cooldown
  :

  LDA controller_last_frame + Controller::down
  CMP controller + Controller::down
  BCS :+
    ;; Increase target scanline by one, and start repeat cooldown
    adc16 target_scanline, #$0001

    ;; Check if target scanline is >=262
    cmp16 target_scanline, #262
    BCC no_reset_target_scanline
      LDA #$00
      STA target_scanline
      STA target_scanline + 1
    no_reset_target_scanline:

    LDA #$18
    STA target_scanline_increase_repeat_cooldown
  :

  LDA controller + Controller::down
  BEQ :++
    LDA target_scanline_increase_repeat_cooldown
    BNE :+
      ;; It has been some time since down was first being held, so just increment the target scanline
      adc16 target_scanline, #$0001
      ;; Check if target scanline is >=262
      cmp16 target_scanline, #262
      BCC no_reset_target_scanline2
        LDA #$00
        STA target_scanline
        STA target_scanline + 1
      no_reset_target_scanline2:

      JMP :++
    :
      DEC target_scanline_increase_repeat_cooldown
  :
  

  LDA controller_last_frame + Controller::up
  CMP controller + Controller::up
  BCS :+
    ;; Increase target scanline by one, and start repeat cooldown
    adc16 target_scanline, #$FFFF

    ;; Check if target scanline is <0
    cmp16 target_scanline, #$FFFF
    BNE no_reset_target_scanline3
      LDA #<261
      STA target_scanline
      LDA #>261
      STA target_scanline + 1
    no_reset_target_scanline3:

    LDA #$18
    STA target_scanline_decrease_repeat_cooldown
  :

  LDA controller + Controller::up
  BEQ :++
    LDA target_scanline_decrease_repeat_cooldown
    BNE :+
      ;; It has been some time since up was first being held, so just decrement the target scanline
      adc16 target_scanline, #$FFFF
      ;; Check if target scanline is <0
      cmp16 target_scanline, #$FFFF
      BNE no_reset_target_scanline4
        LDA #<261
        STA target_scanline
        LDA #>261
        STA target_scanline + 1
      no_reset_target_scanline4:

      JMP :++
    :
      DEC target_scanline_decrease_repeat_cooldown
  :

  LDA pin0_target
  STA VOLTAGES_PIN0
  LDA pin1_target
  STA VOLTAGES_PIN1
  LDA polling_freq
  STA VOLTAGES_POLLING_FREQUENCY
  LDA polling_freq + 1
  STA VOLTAGES_POLLING_FREQUENCY_HIGH
  LDA polling_freq + 2
  STA VOLTAGES_POLLING_FREQUENCY_HIGHER
  LDA display_row
  STA VOLTAGES_DISPLAY_ROW

  LDA target_dot
  STA POLLING_START_X_LO
  LDA target_dot + 1
  STA POLLING_START_X_HI

  LDA target_scanline
  STA POLLING_START_Y_LO
  LDA target_scanline + 1
  STA POLLING_START_Y_HI

  LDA #%00011110
  STA $2001

  LDA holding_start
  BNE @noload
  LDA holding_select
  BNE @noload

  ;; Load if zoom changed
  LDA zoom_change_r
  BNE @initiate_load
  LDA zoom_change_l
  BNE @initiate_load

  LDA controller_last_frame + Controller::a_button
  CMP controller + Controller::a_button
  BCS @noload
  @initiate_load:
    LDA #$01
    STA stuff_loaded
    INC $FE
    LDA #$00
    STA VOLTAGES_COUNTER_RESET
    NOP
    LDA $FE
    STA $8001

    @polling_data_ready:
      ;; Loop until the data is ready
      LDA POLLING_DATA_READY
      BEQ @polling_data_ready

  @noload:

  LDA #%00011110
  STA $2001

  INC $FF

  LDA stuff_loaded
  ORA update_display_row
  BNE :+
    JMP endo
  :
    LDA #$01
    STA VOLTAGES_INDEX_RESET
    LDA #$00
    STA VOLTAGES_INDEX_RESET
    .repeat 128,i
      LDA VOLTAGES
      STA nt_buffer + i
    .endrepeat
  endo:

  LDA stuff_loaded
  ORA update_display_row
  BNE :+
    JMP endo1

  :
    .repeat 128,i
      LDA VOLTAGES
      STA nt_buffer + i + 128
    .endrepeat
  endo1:

  ;; reset update_display_row
  LDA #$00
  STA update_display_row
  STA stuff_loaded

  LDA target_scanline
  STA target_scanline_for_sprite
  LDA target_scanline + 1
  STA target_scanline_for_sprite + 1

  ;; Subtract one to offset for sprite evaluation render annoyance
  adc16 target_scanline_for_sprite, #$FFFF

  ;; Flip with the radix in case there's too many sprites
  LDA even_frame
  BEQ :+
    LDX #$00
    JMP :++
  :
    LDX #$3C
  :
  INX
  ;; Load the sprite index first so we can easily overwrite it if we're on scanline 0
  LDA #$10
  STA $0200, X
  ;; Use target scanline as the Y coord of the cursor, unless it's "offscreen", so just make it 239
  
  LDA target_scanline + 1
  BEQ :+
    ;; sprite is definitely offscreen
    LDA #$EE
    JMP :+++++
  :
    ;; Sprite may be offscreen
    LDA target_scanline
    BNE :+
      ;; sprite should be placed on first scanline
      ;; Overwrite index for "chopped" sprite
      LDA #$11
      STA $0200, X
      LDA #$00

      JMP :+++
    :
    CMP #$F0
    BCC :+
      ;; Sprite is offscreen
      LDA #$EE
      JMP :++
    :
      LDA target_scanline_for_sprite
    :
  :
  DEX
  STA $0200, X
  LDA #$00
  INX
  INX
  STA $0200, X
  ; Check if high byte of target dot is non-zero, if so, just set cursor_x to FF
  LDA target_dot + 1
  BEQ :+
    LDA #$FF
    INX
    STA $0200, X
    JMP :++
  :
    LDA target_dot
    INX
    STA $0200, X
  :

  ;; pin0

  LDA #$3E
  STA $0204
  LDX pin0_target
  LDA pin_displays_high, X
  STA $0205
  ;; set palette to red if there is an error
  LDA #$00
  STA $0206
  LDA #$0F
  STA $0207

  LDA #$3E
  STA $0208
  LDX pin0_target
  LDA pin_displays_low, X
  STA $0209
  ;; set palette to red if there is an error
  LDA #$00
  STA $020A
  LDA #$16
  STA $020B

  ;; pin1

  LDA #($28 + $1E)
  STA $020C
  LDX pin1_target
  LDA pin_displays_high, X
  STA $020D
  LDA #$00
  STA $020E
  LDA #($20 - $11)
  STA $020F

  LDA #($28 + $1E)
  STA $0210
  LDX pin1_target
  LDA pin_displays_low, X
  STA $0211
  LDA #$00
  STA $0212
  LDA #($28 - $12)
  STA $0213

  ; Present the dot and scanline
  LDA target_dot
  STA $00
  LDA target_dot + 1
  STA $01
  
  ;; Convert target_dot to decimal. result is in $02..$04
  JSR HexToDec16

  ;; dot low
  LDA #($18 - 1)
  STA $0200 + $14

  LDA $02
  STA $0200 + $15

  LDA #$00
  STA $0200 + $16

  LDA #($30 + $28 + 8*4 - 2)
  STA $0200 + $17

  ;; dot mid
  LDA #($18 - 1)
  STA $0200 + $18

  LDA $03
  STA $0200 + $19

  LDA #$00
  STA $0200 + $1A

  LDA #($30 + $28 + 8*4 - 8 - 1)
  STA $0200 + $1B

  ;; dot high
  LDA #($18 - 1)
  STA $0200 + $1C

  LDA $04
  STA $0200 + $1D

  LDA #$00
  STA $0200 + $1E

  LDA #($30 + $28 + 8*4 - 16)
  STA $0200 + $1F


  ; Present the dot and scanline
  LDA target_scanline
  STA $00
  LDA target_scanline + 1
  STA $01
  
  ;; Convert target_dot to decimal. result is in $02..$04
  JSR HexToDec16

  ;; scanline low
  LDA #($10 - 1)
  STA $0200 + $20

  LDA $02
  STA $0200 + $21

  LDA #$00
  STA $0200 + $22

  LDA #($30 + $28 + 8*4 - 2)
  STA $0200 + $23

  ;; scanline mid
  LDA #($10 - 1)
  STA $0200 + $24

  LDA $03
  STA $0200 + $25

  LDA #$00
  STA $0200 + $26

  LDA #($30 + $28 + 8*4 - 8 - 1)
  STA $0200 + $27

  ;; scanline high
  LDA #($10 - 1)
  STA $0200 + $28

  LDA $04
  STA $0200 + $29

  LDA #$00
  STA $0200 + $2A

  LDA #($30 + $28 + 8*4 - 16)
  STA $0200 + $2B

  LDA $50D0
  STA alignment_sum
  LDA $50D1
  STA alignment_sum + 1
  LDA $50D2
  STA alignment_sum + 2

  LDA #$00
  STA tens_place
  LDA alignment_sum + 2
  STA ones_place

  LDA ones_place
  CMP #$0A
  BCC :++
  :
    INC tens_place
    SEC
    LDA ones_place
    SBC #$0A
    STA ones_place
    LDA ones_place
    CMP #$0A
    BCS :-
  :
  
  ;; Check how many tenths are in the mantissa
  ONE_TENTH := %0001100110011001
  
  LDA alignment_sum + 1
  STA alignment_sum_fraction_temp + 1
  LDA alignment_sum + 2
  STA alignment_sum_fraction_temp
  
  LDA #$00
  STA tenths_place
  
  check_if_bigger_than_tenth:
  cmp16 alignment_sum_fraction_temp, #ONE_TENTH
  BCC smaller_than_one_tenth
    sbc16 alignment_sum_fraction_temp, #ONE_TENTH
    INC tenths_place
    JMP check_if_bigger_than_tenth
  smaller_than_one_tenth:
  

  ONE_HUNDREDTH := %0000001010001111

  LDA #$00
  STA hundredths_place

  check_if_bigger_than_hundredth:
  cmp16 alignment_sum_fraction_temp, #ONE_HUNDREDTH
  BCC smaller_than_one_hundredth
    sbc16 alignment_sum_fraction_temp, #ONE_HUNDREDTH
    INC hundredths_place
    JMP check_if_bigger_than_hundredth
  smaller_than_one_hundredth:

  LDA tens_place
  BEQ :+
  
    ;; alignment tens
    LDA #($18 - 1)
    STA $0200 + $2C

    LDA tens_place
    STA $0200 + $2D

    LDA #$00
    STA $0200 + $2E

    LDA #($60 + $28 + 8*4 + 2)
    STA $0200 + $2F

    JMP :++
  :
    LDA #$FF
    STA $0200 + $2F
  :
  
  LDA tens_place
  BNE render_ones
    LDA ones_place
    BEQ no_render_ones
      JMP render_ones
    no_render_ones:
      LDA #$FF
      STA $0200 + $33 
    JMP done_rendering_ones
  render_ones:
    ;; alignment ones
    LDA #($18 - 1)
    STA $0200 + $30

    LDA ones_place
    STA $0200 + $31

    LDA #$00
    STA $0200 + $32

    LDA #($68 + $28 + 8*4 + 2)
    STA $0200 + $33
  done_rendering_ones:
  
  ;; alignment tenths
  LDA #($18 - 1)
  STA $0200 + $34

  LDA tenths_place
  STA $0200 + $35

  LDA #$00
  STA $0200 + $36

  LDA #($70 + $28 + 8*4 + 2)
  STA $0200 + $37
  
  ;; hundredths place
  
  LDA #($18 - 1)
  STA $0200 + $38

  LDA hundredths_place
  STA $0200 + $39

  LDA #$00
  STA $0200 + $3A

  LDA #($7B + $28 + 8*4 + 2)
  STA $0200 + $3B

  ;; Render radix
  ;; Flip with the pixel cursor in case there's too many sprites
  LDA even_frame
  BEQ :+
    LDX #$3C
    JMP :++
  :
    LDX #$00
  :
  LDA #($18 - 1)
  STA $0200, X

  LDA #$88
  INX
  STA $0200, X

  LDA #$00
  INX
  STA $0200, X

  LDA #($76 + $28 + 8*4 + 2)
  INX
  STA $0200, X

  LDA cursor_mode
  CMP #RulerCursor
  BEQ display_ruler_cursor
  JMP clear_ruler_cursor
  display_ruler_cursor:
    ;; Load the Ruler cursor and clear the pixel cursor
    
    ;; Flip with the pixel cursor in case there's too many sprites
    LDA even_frame
    BEQ :+
      LDX #$00
      JMP :++
    :
      LDX #$3C
    :

    INX
    ;; Load the sprite index first so we can easily overwrite it if we're on scanline 0
    LDA #$20
    STA $0200, X
    ;; Use target scanline as the Y coord of the cursor, unless it's "offscreen", so just make it 239
    
    LDA target_scanline + 1
    BEQ :+
      ;; sprite is definitely offscreen
      LDA #$EE
      JMP :+++++
    :
      ;; Sprite may be offscreen
      LDA target_scanline
      BNE :+
        ;; sprite should be placed on first scanline
        ;; Overwrite index for "chopped" sprite
        LDA #$11
        STA $0200, X
        LDA #$00

        JMP :+++
      :
      CMP #$F0
      BCC :+
        ;; Sprite is offscreen
        LDA #$EE
        JMP :++
      :
        LDA target_scanline_for_sprite
      :
    :
    DEX
    STA $0200, X
    LDA #$00
    INX
    INX
    STA $0200, X
    ; Check if high byte of target dot is non-zero, if so, just set cursor_x to FF
    LDA target_dot + 1
    BEQ :+
      LDA #$FF
      INX
      STA $0200, X
      JMP :++
    :
      LDA target_dot
      INX
      STA $0200, X
    :
    DEX
    DEX
    DEX
    LDA $0200, X
    STA $0240
    INX
    LDA $0200, X
    CLC
    ADC #$01
    STA $0241
    LDA #$00
    STA $0242
    INX
    INX
    LDA $0200, X
    CLC
    ADC #$08
    STA $0243

    
    LDA $0240 + 0 + 0 + 4*0
    STA $0240 + 0 + 4 + 4*0
    LDY $0240 + 1 + 0 + 4*0
    INY
    STY $0240 + 1 + 4 + 4*0
    LDA $0240 + 2 + 0 + 4*0
    STA $0240 + 2 + 4 + 4*0
    LDA $0240 + 3 + 0 + 4*0
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*0
    
    LDA $0240 + 0 + 0 + 4*1
    STA $0240 + 0 + 4 + 4*1
    LDY $0240 + 1 + 0 + 4*1
    INY
    STY $0240 + 1 + 4 + 4*1
    LDA $0240 + 2 + 0 + 4*1
    STA $0240 + 2 + 4 + 4*1
    LDA $0240 + 3 + 0 + 4*1
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*1
    
    LDA $0240 + 0 + 0 + 4*2
    STA $0240 + 0 + 4 + 4*2
    LDY $0240 + 1 + 0 + 4*2
    INY
    STY $0240 + 1 + 4 + 4*2
    LDA $0240 + 2 + 0 + 4*2
    STA $0240 + 2 + 4 + 4*2
    LDA $0240 + 3 + 0 + 4*2
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*2
    
    LDA $0240 + 0 + 0 + 4*3
    STA $0240 + 0 + 4 + 4*3
    LDY $0240 + 1 + 0 + 4*3
    INY
    STY $0240 + 1 + 4 + 4*3
    LDA $0240 + 2 + 0 + 4*3
    STA $0240 + 2 + 4 + 4*3
    LDA $0240 + 3 + 0 + 4*3
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*3
    
    LDA $0240 + 0 + 0 + 4*4
    STA $0240 + 0 + 4 + 4*4
    LDY $0240 + 1 + 0 + 4*4
    INY
    STY $0240 + 1 + 4 + 4*4
    LDA $0240 + 2 + 0 + 4*4
    STA $0240 + 2 + 4 + 4*4
    LDA $0240 + 3 + 0 + 4*4
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*4
    
    LDA $0240 + 0 + 0 + 4*5
    STA $0240 + 0 + 4 + 4*5
    LDY $0240 + 1 + 0 + 4*5
    INY
    STY $0240 + 1 + 4 + 4*5
    LDA $0240 + 2 + 0 + 4*5
    STA $0240 + 2 + 4 + 4*5
    LDA $0240 + 3 + 0 + 4*5
    CLC
    ADC #$08
    STA $0240 + 3 + 4 + 4*5

    JMP :+
  clear_ruler_cursor:
    ;; Clear the ruler cursor
    LDA #$FF
    STA $0240
    STA $0244
    STA $0248
    STA $024C
    STA $0250
    STA $0254
    STA $0258
  :

  ;; Make a copy of the pin number displays and put them next to the name of the pin
  
  .repeat 4, i
    LDA $0204 + 0 + 4*i
    SEC
    SBC #$1F
    STA $0260 + 0 + 4*i
    LDA $0204 + 1 + 4*i
    STA $0260 + 1 + 4*i
    LDA $0204 + 2 + 4*i
    STA $0260 + 2 + 4*i
    LDA $0204 + 3 + 4*i
    STA $0260 + 3 + 4*i
  .endrepeat

  ;; Check if mapper supports scanline numbers
  ;; This is so that if we're debugging in Mesen, we don't get a bunch of 2001 writes
  ;; We're using the scanline feature to change the colors of the voltage report
  LDA MAPPER_CURRENT_SCANLINE_BUFFER
  CMP #$50
  BEQ no_scanline_support_jumper
  LDA MAPPER_CURRENT_SCANLINE_BUFFER
  BNE scanline_supported
  no_scanline_support_jumper:
    JMP no_scanline_support
  scanline_supported:

  wait_scanline_0:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_HI
    BEQ past_scanline_0
    ORA MAPPER_CURRENT_SCANLINE_LO
    
    BNE wait_scanline_0
  past_scanline_0:

  wait_scanline_63:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #63
    BNE wait_scanline_63
      ;; make it red tinted
      LDA ppu_mask_buffer
      ORA #%00100000
      STA PPU_MASK_2001

  wait_scanline_71:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #71
    BNE wait_scanline_71
      ;; make it blue tinted
      LDA ppu_mask_buffer
      ORA #%11000000
      STA PPU_MASK_2001

  wait_scanline_95:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #95
    BNE wait_scanline_95
      ;; make it red tinted
      LDA ppu_mask_buffer
      ORA #%00100000
      STA PPU_MASK_2001

  wait_scanline_103:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #103
    BNE wait_scanline_103
      ;; make it blue tinted
      LDA ppu_mask_buffer
      ORA #%11000000
      STA PPU_MASK_2001

  wait_scanline_127:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #127
    BNE wait_scanline_127
      ;; make it red tinted
      LDA ppu_mask_buffer
      ORA #%00100000
      STA PPU_MASK_2001

  wait_scanline_135:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #135
    BNE wait_scanline_135
      ;; make it blue tinted
      LDA ppu_mask_buffer
      ORA #%11000000
      STA PPU_MASK_2001

  wait_scanline_159:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #159
    BNE wait_scanline_159
      ;; make it red tinted
      LDA ppu_mask_buffer
      ORA #%00100000
      STA PPU_MASK_2001

  wait_scanline_167:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #167
    BNE wait_scanline_167
      ;; make it blue tinted
      LDA ppu_mask_buffer
      ORA #%11000000
      STA PPU_MASK_2001

  wait_scanline_191:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #191
    BNE wait_scanline_191
      ;; make it red tinted
      LDA ppu_mask_buffer
      ORA #%00100000
      STA PPU_MASK_2001

  wait_scanline_199:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #199
    BNE wait_scanline_199
      ;; make it blue tinted
      LDA ppu_mask_buffer
      ORA #%11000000
      STA PPU_MASK_2001

  wait_scanline_207:
    LDA MAPPER_CURRENT_SCANLINE_BUFFER
    LDA MAPPER_CURRENT_SCANLINE_LO
    CMP #207
    BNE wait_scanline_207
      ;; make it normal colored
      LDA ppu_mask_buffer
      ;ORA #%11000000
      STA PPU_MASK_2001

  no_scanline_support:

Forever:

  ;; idle the engine until the next vblank occurs
  LDA vblank_happened
  BEQ Forever
  LDA #$00
  STA vblank_happened
  JMP Main

pin_displays_hex:
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F


pin_displays_high:
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05

pin_displays_low:
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09

.proc HexToDec16
  input = $00
  result = $02
  p_digit = $06

  ;; Store a pointer to the first digit table
  LDA #<hex_to_dec_512_lo
  STA $06
  LDA #>hex_to_dec_512_lo
  STA $07

  ;; Add input to the pointer to get the correct digit
  adc16 p_digit, input

  ;; get value
  LDY #$00
  LDA (p_digit), Y

  STA result
  
  LDA #<hex_to_dec_512_md
  STA $06
  LDA #>hex_to_dec_512_md
  STA $07
  
  adc16 p_digit, input

  LDY #$00
  LDA (p_digit), Y
  
  STA result + 1
  
  LDA #<hex_to_dec_512_hi
  STA $06
  LDA #>hex_to_dec_512_hi
  STA $07
  
  adc16 p_digit, input

  LDY #$00
  LDA (p_digit), Y
  
  STA result + 2

  RTS

.endproc

hex_to_dec_512_lo:
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09
  .byte $00, $01
hex_to_dec_512_md:
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .byte $07, $07, $07, $07, $07, $07, $07, $07, $07, $07
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $09, $09, $09, $09, $09, $09, $09, $09, $09, $09
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .byte $07, $07, $07, $07, $07, $07, $07, $07, $07, $07
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $09, $09, $09, $09, $09, $09, $09, $09, $09, $09
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .byte $07, $07, $07, $07, $07, $07, $07, $07, $07, $07
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $09, $09, $09, $09, $09, $09, $09, $09, $09, $09
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .byte $07, $07, $07, $07, $07, $07, $07, $07, $07, $07
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $09, $09, $09, $09, $09, $09, $09, $09, $09, $09
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .byte $07, $07, $07, $07, $07, $07, $07, $07, $07, $07
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $09, $09, $09, $09, $09, $09, $09, $09, $09, $09
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01
hex_to_dec_512_hi:
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $03, $03, $03, $03, $03, $03, $03, $03, $03, $03
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .byte $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .byte $05, $05



NMI:

  ;; cache register state to restore before returning from interrupt
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #$01
  STA vblank_happened

  ;; For Alignment timing check
  LDA #$00
  STA PPU_ADDR_2006
  STA PPU_ADDR_2006
  LDA $2007
  STA PPU_DATA_2007

  LDA #$3F
  STA PPU_ADDR_2006
  LDA #$00
  STA PPU_ADDR_2006

  LDA #$0F
  STA PPU_DATA_2007
  LDA #$00
  STA PPU_DATA_2007
  LDA #$0F
  STA PPU_DATA_2007
  LDA #$3D
  STA PPU_DATA_2007

  LDA #$0F
  STA PPU_DATA_2007
  LDA #$20
  STA PPU_DATA_2007
  LDA #$10
  STA PPU_DATA_2007
  LDA #$00
  STA PPU_DATA_2007

  LDA #$3F
  STA PPU_ADDR_2006
  LDA #$11
  STA PPU_ADDR_2006

  LDA #$20
  STA PPU_DATA_2007
  LDA #$10
  STA PPU_DATA_2007
  LDA #$00
  STA PPU_DATA_2007


  LDA nt_buffer_chunk
  CMP #$00
  BEQ @chunk0jumper
  CMP #$01
  BEQ @chunk1jumper
  CMP #$02
  BEQ @chunk2jumper
  CMP #$03
  BEQ @chunk3jumper
  CMP #$04
  BEQ @chunk4jumper
  @chunk0jumper:
    JMP @chunk0
  @chunk1jumper:
    JMP @chunk1
  @chunk2jumper:
    JMP @chunk2
  @chunk3jumper:
    JMP @chunk3
  @chunk4jumper:
    JMP @chunk4

  @chunk0:

    .repeat 8
      NOP
    .endrepeat

    LDA #$21
    STA PPU_ADDR_2006
    LDA #$02
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i
      STA PPU_DATA_2007
    .endrepeat

    LDA #$21
    STA PPU_ADDR_2006
    LDA #$22
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 128
      STA PPU_DATA_2007
    .endrepeat

    JMP @doneChunks

  @chunk1:

    .repeat 6
      NOP
    .endrepeat

    LDA #$21
    STA PPU_ADDR_2006
    LDA #$82
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 24
      STA PPU_DATA_2007
    .endrepeat

    LDA #$21
    STA PPU_ADDR_2006
    LDA #$A2
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 128 + 24
      STA PPU_DATA_2007
    .endrepeat

    JMP @doneChunks

  @chunk2:

    .repeat 4
      NOP
    .endrepeat

    LDA #$22
    STA PPU_ADDR_2006
    LDA #$02
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 24*2
      STA PPU_DATA_2007
    .endrepeat

    LDA #$22
    STA PPU_ADDR_2006
    LDA #$22
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 128 + 24*2
      STA PPU_DATA_2007
    .endrepeat

    JMP @doneChunks

  @chunk3:

    .repeat 2
      NOP
    .endrepeat

    LDA #$22
    STA PPU_ADDR_2006
    LDA #$82
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 24*3
      STA PPU_DATA_2007
    .endrepeat

    LDA #$22
    STA PPU_ADDR_2006
    LDA #$A2
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 128 + 24*3
      STA PPU_DATA_2007
    .endrepeat

    JMP @doneChunks

  @chunk4:

    LDA #$23
    STA PPU_ADDR_2006
    LDA #$02
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 24*4
      STA PPU_DATA_2007
    .endrepeat

    LDA #$23
    STA PPU_ADDR_2006
    LDA #$22
    STA PPU_ADDR_2006

    .repeat 24,i
      LDA nt_buffer + i + 128 + 24*4
      STA PPU_DATA_2007
    .endrepeat

  @doneChunks:

  INC nt_buffer_chunk
  LDA nt_buffer_chunk
  CMP #$05
  BNE :+
    LDA #$00
    STA nt_buffer_chunk
  :
  ;; OAMDMA
  JSR OAMDMA

  LDA update_pin_0_text
  EOR #$01
  STA update_pin_0_text

  LDA update_pin_0_text
  BNE update_pin_1_text

  ;; Write pin names to nametables

  ;; Get a pointer to the list of pin name pointers
  LDA #<pin_name_strings
  STA $20
  LDA #>pin_name_strings
  STA $21
  LDA pin0_target
  ASL
  TAY
  LDA ($20), Y
  STA $22
  INY
  LDA ($20), Y
  STA $23
  
  ;; $22,$23 is now a pointer to the string
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$82
  STA PPU_ADDR_2006
  LDY #$00
  :
    LDA ($22), Y
    BEQ :+
    STA PPU_DATA_2007
    INY
    JMP :-
  :

  JMP done_updating_pin_text

  update_pin_1_text:

  ;; Write pin names to nametables

  ;; Get a pointer to the list of pin name pointers
  LDA #<pin_name_strings
  STA $20
  LDA #>pin_name_strings
  STA $21
  LDA pin1_target
  ASL
  TAY
  LDA ($20), Y
  STA $22
  INY
  LDA ($20), Y
  STA $23
  
  ;; $22,$23 is now a pointer to the string
  
  LDA #$20
  STA PPU_ADDR_2006
  LDA #$A2
  STA PPU_ADDR_2006
  LDY #$00
  :
    LDA ($22), Y
    BEQ :+
    STA PPU_DATA_2007
    INY
    JMP :-
  :

  done_updating_pin_text:

  JSR ControllerInput

  LDA even_frame
  EOR #$01
  STA even_frame

  LDA #$F0
  STA $2005
  LDA #$00
  STA $2005
  LDA ppu_mask_buffer
  STA PPU_MASK_2001
  LDA ppu_ctrl_buffer
  STA PPU_CTRL_2000
  ;; restore register state
  PLA
  TAY
  PLA
  TAX
  PLA
  RTI

BREAK:

  RTI

display_attributes:
  .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %11110011, %11110000, %11110000, %11110000, %11110000, %11110000, %11111100, %11111111
  .byte %11110011, %11110000, %11110000, %11110000, %11110000, %11110000, %11111100, %11111111
  .byte %11110011, %11110000, %11110000, %11110000, %11110000, %11110000, %11111100, %11111111
  .byte %11110011, %11110000, %11110000, %11110000, %11110000, %11110000, %11111100, %11111111
  .byte %11110011, %11110000, %11110000, %11110000, %11110000, %11110000, %11111100, %11111111
  .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

pin_name_strings:
  .word pin_00_name
  .word pin_01_name
  .word pin_02_name
  .word pin_03_name
  .word pin_04_name
  .word pin_05_name
  .word pin_06_name
  .word pin_07_name
  .word pin_08_name
  .word pin_09_name
  .word pin_10_name
  .word pin_11_name
  .word pin_12_name
  .word pin_13_name
  .word pin_14_name
  .word pin_15_name
  .word pin_16_name
  .word pin_17_name
  .word pin_18_name
  .word pin_19_name
  .word pin_20_name
  .word pin_21_name
  .word pin_22_name
  .word pin_23_name
  .word pin_24_name
  .word pin_25_name
  .word pin_26_name
  .word pin_27_name
  .word pin_28_name
  .word pin_29_name
  .word pin_30_name
  .word pin_31_name
  .word pin_32_name
  .word pin_33_name
  .word pin_34_name
  .word pin_35_name
  .word pin_36_name
  .word pin_37_name
  .word pin_38_name
  .word pin_39_name
  .word pin_40_name
  .word pin_41_name
  .word pin_42_name
  .word pin_43_name
  .word pin_44_name
  .word pin_45_name
  .word pin_46_name
  .word pin_47_name
  .word pin_48_name
  .word pin_49_name
  .word pin_50_name
  .word pin_51_name
  .word pin_52_name
  .word pin_53_name
  .word pin_54_name
  .word pin_55_name
  .word pin_56_name

  pin_00_name: .asciiz "M2            "
  pin_01_name: .asciiz "CPU R/W       "
  pin_02_name: .asciiz "CPU A0        "
  pin_03_name: .asciiz "CPU A1        "
  pin_04_name: .asciiz "CPU A2        "
  pin_05_name: .asciiz "CPU A3        "
  pin_06_name: .asciiz "CPU A4        "
  pin_07_name: .asciiz "CPU A5        "
  pin_08_name: .asciiz "CPU A6        "
  pin_09_name: .asciiz "CPU A7        "
  pin_10_name: .asciiz "CPU A8        "
  pin_11_name: .asciiz "CPU A9        "
  pin_12_name: .asciiz "CPU A10       "
  pin_13_name: .asciiz "CPU A11       "
  pin_14_name: .asciiz "CPU A12       "
  pin_15_name: .asciiz "CPU A13       "
  pin_16_name: .asciiz "CPU A14       "
  pin_17_name: .asciiz "CPU A15       "
  pin_18_name: .asciiz "CPU D0        "
  pin_19_name: .asciiz "CPU D1        "
  pin_20_name: .asciiz "CPU D2        "
  pin_21_name: .asciiz "CPU D3        "
  pin_22_name: .asciiz "CPU D4        "
  pin_23_name: .asciiz "CPU D5        "
  pin_24_name: .asciiz "CPU D6        "
  pin_25_name: .asciiz "CPU D7        "
  pin_26_name: .asciiz "/IRQ          "
  pin_27_name: .asciiz "PPU /RD       "
  pin_28_name: .asciiz "PPU /WR       "
  pin_29_name: .asciiz "PPU A0        "
  pin_30_name: .asciiz "PPU A1        "
  pin_31_name: .asciiz "PPU A2        "
  pin_32_name: .asciiz "PPU A3        "
  pin_33_name: .asciiz "PPU A4        "
  pin_34_name: .asciiz "PPU A5        "
  pin_35_name: .asciiz "PPU A6        "
  pin_36_name: .asciiz "PPU A7        "
  pin_37_name: .asciiz "PPU A8        "
  pin_38_name: .asciiz "PPU A9        "
  pin_39_name: .asciiz "PPU A10       "
  pin_40_name: .asciiz "PPU A11       "
  pin_41_name: .asciiz "PPU A12       "
  pin_42_name: .asciiz "PPU A13       "
  pin_43_name: .asciiz "PPU /A13      "
  pin_44_name: .asciiz "PPU D0        "
  pin_45_name: .asciiz "PPU D1        "
  pin_46_name: .asciiz "PPU D2        "
  pin_47_name: .asciiz "PPU D3        "
  pin_48_name: .asciiz "PPU D4        "
  pin_49_name: .asciiz "PPU D5        "
  pin_50_name: .asciiz "PPU D6        "
  pin_51_name: .asciiz "PPU D7        "
  pin_52_name: .asciiz "CIRAM /CE     "
  pin_53_name: .asciiz "CIRAM A10     "
  pin_54_name: .asciiz "PPU ADDR in NT"
  pin_55_name: .asciiz "PPU ADDR in AT"
  pin_56_name: .asciiz "MAP CPU OE    "

.segment "VECTORS"
  .word NMI
  .word RESET
  .word BREAK

.segment "CHARS"
  .incbin "voltages.chr"
  .incbin "letter_ui.chr"
  .incbin "voltages_solid.chr"