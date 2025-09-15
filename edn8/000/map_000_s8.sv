 
module map_000_s8(

	input  MapIn  mai,
	output MapOut mao
);
//************************************************************* base header
	CpuBus cpu;
	PpuBus ppu;
	SysCfg cfg;
	SSTBus sst;
	assign cpu = mai.cpu;
	assign ppu = mai.ppu;
	assign cfg = mai.cfg;
	assign sst = mai.sst;
	
	MemCtrl prg;
	MemCtrl chr;
	MemCtrl srm;
	assign mao.prg = prg;
	assign mao.chr = chr;
	assign mao.srm = srm;

	assign prg.dati			= cpu.data;
	assign chr.dati			= ppu.data;
	assign srm.dati			= cpu.data;
	
	wire int_cpu_oe = !cpu.m2 | !cpu.rw ? 0 :
    voltages_oe_raw
    | polling_data_ready_oe
    | current_scanline_support_oe
    | current_scanline_hi_oe
    | current_scanline_lo_oe
    | skew_count_oe
    | skew_count_lo_oe
    | skew_count_md_oe
    | skew_count_hi_oe;
	wire int_ppu_oe;
	wire [7:0]int_cpu_data =
    polling_data_ready_oe ? {7'b0000000, polling_data_ready} :
    voltages_oe_raw ? voltages_out_dat :
    current_scanline_support_oe ? 8'h01 :
    current_scanline_hi_oe ? {7'b0000000, current_scanline_buffer[0]} :
    current_scanline_lo_oe ? current_scanline_buffer[7:0] :
    skew_count_oe ? {2'b00, ppu_wr_m2_skew_count[5:0]} :
    skew_count_lo_oe ? {alignment_sum[1:0], 6'b000000} :
    skew_count_md_oe ? {alignment_sum[9:2]} :
    skew_count_hi_oe ? {alignment_sum[17:10]} :
    8'h00;
	wire [7:0]int_ppu_data;
	
	assign mao.map_cpu_oe	= int_cpu_oe | (srm.ce & srm.oe) | (prg.ce & prg.oe);
	assign mao.map_cpu_do	= int_cpu_oe ? int_cpu_data : srm.ce ? mai.srm_do : mai.prg_do;
	
	assign mao.map_ppu_oe	= int_ppu_oe | (chr.ce & chr.oe);
	assign mao.map_ppu_do	= int_ppu_oe ? int_ppu_data : mai.chr_do;
//************************************************************* configuration
	assign mao.prg_mask_off = 0;
	assign mao.chr_mask_off = 0;
	assign mao.srm_mask_off = 0;
	assign mao.mir_4sc		= 0;//enable support for 4-screen mirroring. for activation should be enabled in cfg.mir_4 also
	assign mao.bus_cf 		= 0;//bus conflicts
//************************************************************* save state regs read
	assign mao.sst_di[7:0] = 
	sst.addr[7:0] == 127 ? cfg.map_idx : 8'hff;
//************************************************************* mapper-controlled pins
	assign srm.ce				= {cpu.addr[15:13], 13'd0} == 16'h6000;
	assign srm.oe				= cpu.rw;
	assign srm.we				= !cpu.rw;
	assign srm.addr[12:0]	= cpu.addr[12:0];
	
	assign prg.ce				= cpu.addr[15];
	assign prg.oe 				= cpu.rw;
	assign prg.we				= 0;
	assign prg.addr[14:0]	= cpu.addr[14:0];
	
	assign chr.ce 				= mao.ciram_ce;
	assign chr.oe 				= !ppu.oe;
	assign chr.we 				= cfg.chr_ram ? !ppu.we & mao.ciram_ce : 0;
	assign chr.addr[14:0]	= {(current_scanline < 64 ? 2'b01 : chr_bank), ppu.addr[12:0]};

	
	//A10-Vmir, A11-Hmir
	assign mao.ciram_a10 	= cfg.mir_v ? ppu.addr[10] : ppu.addr[11];
	assign mao.ciram_ce 		= !ppu.addr[13];
	
	assign mao.irq				= 0;
//************************************************************* mapper implementation

	
	//************************************************************* read voltages from data store
	reg [159:0]voltages_oe_raw_st;
  wire polling_data_ready_oe = cpu.rw & cpu.addr[15:0] == 16'h500E;
	wire voltages_oe_raw = cpu.rw & cpu.addr[15:0] == 16'h5001;
	wire voltages_oe = cpu.rw & cpu.addr[15:0] == 16'h5001 & voltages_oe_raw_st[159:30] == 0;
  wire current_scanline_support_oe = cpu.rw & cpu.addr[15:0] == 16'h5010;
  wire current_scanline_hi_oe = cpu.rw & cpu.addr[15:0] == 16'h5011;
  wire current_scanline_lo_oe = cpu.rw & cpu.addr[15:0] == 16'h5012;
  wire skew_count_oe = cpu.rw & cpu.addr[15:0] == 16'h50CF;
  
  wire skew_count_lo_oe = cpu.rw & cpu.addr[15:0] == 16'h50D0;
  wire skew_count_md_oe = cpu.rw & cpu.addr[15:0] == 16'h50D1;
  wire skew_count_hi_oe = cpu.rw & cpu.addr[15:0] == 16'h50D2;

	
	
  
  // Be able to bank 4 different CHRs
  reg [1:0]chr_bank;
	
	reg [29:0]cpu_addrs_st;
	reg [29:0]cpu_addrs_st_2;
	reg [29:0]cpu_addrs_st_3;
	reg [4:0]cpu_addrs_idx_next;
	reg [9:0]voltage_idx;
	reg [9:0]voltage_idx_pin0;
	reg [9:0]voltage_idx_pin1;
	reg [7:0]voltage_idx_acc;
	reg voltage_idx_reset;
	reg [7:0]voltages_display_row;
	
	reg [7:0]m2_st_8;
	
	//reg [7:0]voltages[256];
	reg [7:0]next_voltage_pin0;
	reg [7:0]next_voltage_pin0_addr;
	reg [7:0]next_voltage_pin1;
	reg [7:0]next_voltage_pin1_addr;
	reg [7:0]next_voltage_pin0_buffer;
	reg [7:0]next_voltage_pin1_buffer;
	reg [7:0]voltages_addr;
	reg [7:0]voltages_addr_pin0;
	reg [7:0]voltages_addr_pin1;
	reg [7:0]voltages_in_dat;
	reg [7:0]voltages_in_dat_pin0;
	reg [7:0]voltages_in_dat_pin1;
	wire [7:0]voltages_out_dat_pin0;
	wire [7:0]voltages_out_dat_pin1;
	wire [7:0]voltages_out_dat;
	reg [7:0]voltages_out_addr;
	reg [11:0]voltages_counter;
	reg voltages_we;
	reg voltages_counter_reset;
	reg [7:0]pin_to_read0;
	reg [7:0]pin_to_read1;
	reg [6:0]voltages_oe_st;
	reg [3:0]voltages_reset_st;
	reg [7:0]ppu_oe_st;
  reg [74:0]ppu_oe_long_st;
	
	wire [56:0]pins = {
	
		/* 56 */ mao.map_cpu_oe,
		/* 55 */ ppu_atr_ce,
		/* 54 */ ppu_ntb_ce,
		/* 53 */ mao.ciram_a10,
		/* 52 */ mao.ciram_ce,
		/* 51 */ ppu.data[7],
		/* 50 */ ppu.data[6],
		/* 49 */ ppu.data[5],
		/* 48 */ ppu.data[4],
		/* 47 */ ppu.data[3],
		/* 46 */ ppu.data[2],
		/* 45 */ ppu.data[1],
		/* 44 */ ppu.data[0],
		/* 43 */ !ppu.addr[13],
		/* 42 */ ppu.addr[13],
		/* 41 */ ppu.addr[12],
		/* 40 */ ppu.addr[11],
		/* 39 */ ppu.addr[10],
		/* 38 */ ppu.addr[9],
		/* 37 */ ppu.addr[8],
		/* 36 */ ppu.addr[7],
		/* 35 */ ppu.addr[6],
		/* 34 */ ppu.addr[5],
		/* 33 */ ppu.addr[4],
		/* 32 */ ppu.addr[3],
		/* 31 */ ppu.addr[2],
		/* 30 */ ppu.addr[1],
		/* 29 */ ppu.addr[0],
		/* 28 */ ppu.we,
		/* 27 */ ppu.oe,
		/* 26 */ mao.irq,
		/* 25 */ cpu.data[7],
		/* 24 */ cpu.data[6],
		/* 23 */ cpu.data[5],
		/* 22 */ cpu.data[4],
		/* 21 */ cpu.data[3],
		/* 20 */ cpu.data[2],
		/* 19 */ cpu.data[1],
		/* 18 */ cpu.data[0],
		/* 17 */ cpu.addr[15],
		/* 16 */ cpu.addr[14],
		/* 15 */ cpu.addr[13],
		/* 14 */ cpu.addr[12],
		/* 13 */ cpu.addr[11],
		/* 12 */ cpu.addr[10],
		/* 11 */ cpu.addr[9],
		/* 10 */ cpu.addr[8],
		/*  9 */ cpu.addr[7],
		/*  8 */ cpu.addr[6],
		/*  7 */ cpu.addr[5],
		/*  6 */ cpu.addr[4],
		/*  5 */ cpu.addr[3],
		/*  4 */ cpu.addr[2],
		/*  3 */ cpu.addr[1],
		/*  2 */ cpu.addr[0],
		/*  1 */ cpu.rw,
		/*  0 */ cpu.m2
	};
	
	// ppu address is referring to a nametable byte, but not an attribute byte
	wire ppu_ntb_ce = ppu.addr[13] == 1 & ppu.addr[9:6] != 4'b1111;
	wire ppu_atr_ce = ppu.addr[13] == 1 & ppu.addr[9:6] == 4'b1111;
	
	assign voltage_read_pin0 = pins[pin_to_read0];
	assign voltage_read_pin1 = pins[pin_to_read1];
	
	reg [23:0]polling_frequency;
	reg [23:0]polling_ctr;
	wire new_clk_100;
	clk_100 fancy_clk(
		.inclk0(mai.clk),
		.c0(new_clk_100));
	
	assign voltages_out_dat[7:0] = voltage_idx_acc[7] ? voltages_out_dat_pin1[7:0] : voltages_out_dat_pin0[7:0];
		
	ram_dp_old voltages_ram_pin0(
		.din_a(voltages_in_dat_pin0[7:0]), 
		.addr_a(voltages_addr_pin0[7:0]), 
		.we_a(voltages_we),
		.clk_a(mai.clk),
		.addr_b(voltage_idx_pin0),
		.dout_b(voltages_out_dat_pin0[7:0]), 
		.clk_b(mai.clk)
	);
		
	ram_dp_old voltages_ram_pin1(
		.din_a(voltages_in_dat_pin1[7:0]), 
		.addr_a(voltages_addr_pin1[7:0]), 
		.we_a(voltages_we),
		.clk_a(mai.clk),
		.addr_b(voltage_idx_pin1),
		.dout_b(voltages_out_dat_pin1[7:0]), 
		.clk_b(mai.clk)
	);
	
  // TODO: double the number of samples to increase precision
	ram_dp_old alignment_samples(
		.din_a(alignment_din[7:0]), 
		.addr_a(alignment_addr[9:0]), 
		.we_a(alignment_we),
		.clk_a(mai.clk),
		.addr_b(alignment_addr[9:0]),
		.dout_b(alignment_dout[7:0]), 
		.clk_b(mai.clk)
	);

  // Can be 0..340
  reg [8:0]polling_start_dot;
  // Can be 0..261
  reg [8:0]polling_start_scanline;

  reg nmi_started;
  reg polling_started;
  reg polling_data_ready;

  reg recalculate_target_dot;
  reg recalculate_target_clk_100;
  reg [23:0]target_dot;
  reg [32:0]target_clk_100;

  reg recalculate_target_scanline;
  reg recalculate_target_clk_100_for_scanline;
  reg [31:0]target_scanline;

  reg [8:0]current_scanline_buffer;
	
	initial begin
    ppu_oe_long_st <= 0;

    polling_start_dot <= 0;
    polling_start_scanline <= 0;

    nmi_started <= 0;
    polling_started <= 0;
    polling_data_ready <= 0;

    recalculate_target_dot <= 0;
    recalculate_target_clk_100 <= 0;
    target_dot <= 0;
    target_clk_100 <= 0;

    recalculate_target_scanline <= 0;
    recalculate_target_clk_100_for_scanline <= 0;
    target_scanline <= 0;

    current_scanline_buffer <= 0;

    chr_bank <= 0;
	end

  always @(negedge cpu.m2) begin
    // Check if nmi started by seeing if the address is ever FFFA
    
    if(cpu.addr[15:0] == 16'hFFFA) begin
      nmi_started <= 1'b1;
    end else begin
      nmi_started <= 1'b0;
    end

    if(cpu.addr[15:0] == 16'h5003) begin
      polling_started <= 1'b1;
      polling_data_ready <= 0;
    end else begin
      if(polling_data_filled == 1) begin
        polling_data_ready <= 1'b1;
      end
      polling_started <= 1'b0;
    end

    if(!cpu.rw & cpu.addr[15:0] == 16'h500A)polling_start_dot[8] <= cpu.data[0];
    if(!cpu.rw & cpu.addr[15:0] == 16'h500B)begin 
      polling_start_dot[7:0] <= cpu.data[7:0];
      recalculate_target_dot <= 1;
    end else begin
      recalculate_target_dot <= 0;
    end

    
    if(!cpu.rw & cpu.addr[15:0] == 16'h500C)polling_start_scanline[8] <= cpu.data[0];
    if(!cpu.rw & cpu.addr[15:0] == 16'h500D)begin 
      polling_start_scanline[7:0] <= cpu.data[7:0];
      recalculate_target_scanline <= 1;
    end else begin
      recalculate_target_scanline <= 0;
    end

    if(cpu.rw & cpu.addr[15:0] == 16'h5010) begin
      // buffer scanline number since we need to read multiple bytes
      current_scanline_buffer[8:0] <= current_scanline[8:0];
    end

    if(!cpu.rw & cpu.addr[15:0] == 16'h5020) begin
      // buffer scanline number since we need to read multiple bytes
      chr_bank[1:0] <= cpu.data[1:0];
    end

    
    
    if(recalculate_target_dot) begin
      // 18.624338624338 ~= 10010.1001111111010100101001
      // target_dot can be
      // *0 == 00000000000000000000000
      // *1 == 00000000100101001111111
      // *2 == 00000001001010011111110
      // *3 == 00000001101111101111101
      // *4 == 00000010010100111111100
      // *5 == 00000010111010001111011
      // *6 == 00000011011111011111010
      target_dot[23:0] <= (polling_start_dot[8:0] * 15'b100101001111111);
      recalculate_target_clk_100 <= 1;
    end

    if(recalculate_target_scanline) begin
      // 1100011001110.1110011001000011101110011001000011101110011001...
      //if(polling_start_scanline == 1) begin
      //  target_scanline[21:0] <= 13'b1100011001110;
      //end else begin
      //  target_scanline[21:0] <= 0;
      //end
      target_scanline[31:0] <= (polling_start_scanline[8:0] * 23'b11000110011101111101110);
      recalculate_target_clk_100_for_scanline <= 1;
    end

    if(recalculate_target_clk_100 | recalculate_target_clk_100_for_scanline) begin
      // 1656547 - presumably first scanline + 261.5 lines
      // 1516835 - presumably the previous - 22 scanlines
      // 1510484 - presumably the previous - 1 scanline
      //  146073 - presumably 23 scanlines
      //  133371 - presumably 21 scanlines
      //  139722 - presumably 22 scanlines
      //  146032 - clks between last scanline and first dot next frame
      if(target_dot[23:0] + (139722 + 6350 - 56 + 52 - 18)*1024 + target_scanline[31:0] > 1663935*1024) begin
        target_clk_100 <= target_dot[23:0] + (139722 + 6350 - 56 + 52 - 18)*1024 + target_scanline[31:0] - 1703870117;
      end else begin
        target_clk_100 <= target_dot[23:0] + (139722 + 6350 - 56 + 52 - 18)*1024 + target_scanline[31:0];
      end
      if(recalculate_target_clk_100) recalculate_target_clk_100 <= 0;
      if(recalculate_target_clk_100_for_scanline) recalculate_target_clk_100_for_scanline <= 0;
    end

  end
	
	always @(posedge mai.clk) begin
	
		m2_st_8[7:0] <= {m2_st_8[6:0], cpu.m2};
		
		if(m2_st_8[7:0] == 8'b01111111) begin
		
			if(mai.map_rst) begin
				pin_to_read0 = 0;
				pin_to_read1 = 2;
				polling_frequency = 0;
			end else begin
				if(!cpu.rw & cpu.addr[15:0] == 16'h5000) begin
          pin_to_read0 <= cpu.data[7:0];
        end
				if(!cpu.rw & cpu.addr[15:0] == 16'h5002)voltage_idx_reset <= cpu.data[0];
				if(!cpu.rw & cpu.addr[15:0] == 16'h5003)voltages_counter_reset <= cpu.data[0];
				if(!cpu.rw & cpu.addr[15:0] == 16'h5004) begin
          pin_to_read1 <= cpu.data[7:0];
        end
				if(!cpu.rw & cpu.addr[15:0] == 16'h5005)polling_frequency[7:0] <= cpu.data[7:0];
				if(!cpu.rw & cpu.addr[15:0] == 16'h5007)voltages_display_row <= cpu.data[7:0];
				if(!cpu.rw & cpu.addr[15:0] == 16'h5008)polling_frequency[15:8] <= cpu.data[7:0];
				if(!cpu.rw & cpu.addr[15:0] == 16'h5009)polling_frequency[23:16] <= cpu.data[7:0];

			end
		end
		
		voltages_oe_raw_st[159:0] <= {voltages_oe_raw_st[158:0], voltages_oe_raw};
	
		if(mai.map_rst) begin
			cpu_addrs_st <= 0;
			cpu_addrs_st_2 <= 0;
			cpu_addrs_st_3 <= 0;
		end else begin
			cpu_addrs_st[29:0] <= {cpu_addrs_st[28:0], cpu.addr[0] & cpu.rw};
			cpu_addrs_st_2[29:0] <= {cpu_addrs_st_2[28:0], cpu.addr[1] & cpu.rw};
			cpu_addrs_st_3[29:0] <= {cpu_addrs_st_3[28:0], cpu.addr[15] & cpu.rw};
		end
		voltages_oe_st[3:0] <= {voltages_oe_st[2:0], voltages_oe};
		
		if(voltage_idx_reset) begin
			voltage_idx_acc <= 0;
			voltage_idx_pin0 <= voltages_display_row * 24 > 1024 ? 0 : voltages_display_row * 24;
			voltage_idx_pin1 <= voltages_display_row * 24 > 1024 ? 0 : voltages_display_row * 24;
			voltage_idx <= voltages_display_row * 24 > 1024 ? 0 : voltages_display_row * 24;
		end
		
		if (voltages_oe_st[3:0] == 4'b0111) begin
			// trigger an increment of the voltage_idx
			voltage_idx <= voltage_idx + 1;
			if(voltage_idx_acc[7]) begin
				voltage_idx_pin1 <= voltage_idx_pin1 + 1;
			end else begin
				voltage_idx_pin0 <= voltage_idx_pin0 + 1;
			end
			voltage_idx_acc <= voltage_idx_acc + 1;
		end
	end

  reg [7:0]scanline_dot_0_cooldown;
  // ~1663898 of these per frame
  // ~6351    of these per scanline
  reg [23:0]frame_clock_100_ticks;

  reg [23:0]clks_since_nmi_started;

  // shift register polling nmi_started
  reg [3:0]nmi_started_st;
  reg watch_for_first_scanline;
  reg watch_for_last_scanline;
  reg [23:0]clks_watching_for_last_scanline;
  reg [23:0]clks_watching_for_first_scanline;

  reg [3:0]polling_started_st;
  reg start_polling;
  reg polling_data_filled;

  reg [8:0]current_scanline;
  reg [20:0]ppu_read_counter;
  reg [23:0]clks_since_ppu_read;
  reg [23:0]clks_since_last_scanline;

  initial begin
    scanline_dot_0_cooldown <= 0;
    frame_clock_100_ticks <= 0;
    nmi_started_st <= 0;
    watch_for_first_scanline <= 0;
    watch_for_last_scanline <= 0;
    clks_watching_for_last_scanline <= 0;
    clks_watching_for_first_scanline <= 0;
    polling_started_st <= 0;
    start_polling <= 0;
    polling_data_filled <= 0;
    clks_since_nmi_started <= 0;
    ppu_read_counter <= 0;
    clks_since_ppu_read <= 0;
    clks_since_last_scanline <= 0;
  end
	
	always @(negedge new_clk_100) begin
	
		ppu_oe_st[7:0] <= {ppu_oe_st[6:0], ppu.oe};
    ppu_oe_long_st[74:0] <= {ppu_oe_long_st[73:0], ppu.oe};
    nmi_started_st[3:0] <= {nmi_started_st[2:0], nmi_started};
    polling_started_st[3:0] <= {polling_started_st[2:0], polling_started};

    if(nmi_started_st == 4'b0001) begin
      clks_since_nmi_started <= 0;
    end else begin
      clks_since_nmi_started <= clks_since_nmi_started + 1;
    end

    if(polling_started_st == 4'b0001 && start_polling == 0) begin
      start_polling <= 1'b1;
      polling_data_filled <= 1'b0;
    end

    if(nmi_started_st == 4'b0001) begin
      ppu_read_counter <= 0;
    end else begin
      if(ppu_oe_st[3:0] == 4'b1110) begin
        ppu_read_counter <= ppu_read_counter + 1;
        clks_since_ppu_read <= 0;
      end else begin
        clks_since_ppu_read <= clks_since_ppu_read + 1;
      end
    end

    if(ppu_read_counter == 170) begin
      current_scanline <= 0;
      clks_since_last_scanline <= 0;
    end else begin

      if(clks_since_last_scanline == 6351) begin
        current_scanline <= current_scanline + 1;
        clks_since_last_scanline <= 0;
      end else begin
        clks_since_last_scanline <= clks_since_last_scanline + 1;
      end
    end


    if(scanline_dot_0_cooldown != 0) begin
      scanline_dot_0_cooldown <= scanline_dot_0_cooldown - 1;

      frame_clock_100_ticks <= frame_clock_100_ticks + 1;
      
      if(nmi_started_st == 4'b0001) begin
        // This is 10ns after it started
        watch_for_first_scanline <= 1;
        watch_for_last_scanline <= 1;
        clks_watching_for_last_scanline <= 0;
        clks_watching_for_first_scanline <= 0;
      end else begin
        clks_watching_for_last_scanline <= clks_watching_for_last_scanline + 1;
        clks_watching_for_first_scanline <= clks_watching_for_first_scanline + 1;
      end
    end else if(ppu_oe_long_st[0] == 1'b0 && ppu_oe_long_st[9] == 1'b1 && ppu_oe_long_st[28] == 1'b1 && ppu_oe_long_st[46] == 1'b0) begin
      scanline_dot_0_cooldown <= 100;
      
      if(nmi_started_st == 4'b0001) begin
        // This is 10ns after it started
        watch_for_first_scanline <= 1;
        watch_for_last_scanline <= 1;
        clks_watching_for_last_scanline <= 0;
        clks_watching_for_first_scanline <= 0;
        frame_clock_100_ticks <= frame_clock_100_ticks + 1;
      end else begin
        if(watch_for_first_scanline == 1) begin
          // Waiting for the first scanline, now we don't need to wait any more
          watch_for_first_scanline <= 0;
        end

        if(clks_watching_for_last_scanline > 1663898 - 2*6351 - 1000) begin

          frame_clock_100_ticks <= 0;
          clks_watching_for_last_scanline <= 0;
        end else begin
          clks_watching_for_last_scanline <= clks_watching_for_last_scanline + 1;
          frame_clock_100_ticks <= frame_clock_100_ticks + 1;
        end

        if(watch_for_first_scanline == 1 && clks_watching_for_first_scanline > 1663898 - 2*6351 - 1000 - 240*6351) begin
          clks_watching_for_first_scanline <= 0;
        end else begin
          clks_watching_for_first_scanline <= clks_watching_for_first_scanline + 1;
        end
      end
    end else begin
      
      if(nmi_started_st == 4'b0001) begin
        // This is 10ns after it started
        watch_for_first_scanline <= 1;
        watch_for_last_scanline <= 1;
        clks_watching_for_last_scanline <= 0;
        clks_watching_for_first_scanline <= 0;
      end else begin
        clks_watching_for_last_scanline <= clks_watching_for_last_scanline + 1;
        clks_watching_for_first_scanline <= clks_watching_for_first_scanline + 1;
      end
      frame_clock_100_ticks <= frame_clock_100_ticks + 1;
    end
		
		
		if (frame_clock_100_ticks == target_clk_100[32:10] && start_polling == 1) begin
    //if (clks_since_nmi_started == 1663898 - 2*6351 - 1000 && start_polling == 1) begin
			polling_ctr <= 0;
			voltages_counter <= 0;
      start_polling <= 0;
			next_voltage_pin0 <= {7'b0, voltage_read_pin0};
			next_voltage_pin1 <= {7'b0, voltage_read_pin1};
		end else begin
			 if (polling_ctr == polling_frequency) begin
				if (voltages_counter < 2048) begin
          if(voltages_counter == 2047) begin
            // Filling the last value
            polling_data_filled <= 1'b1;
          end
					voltages_counter <= voltages_counter + 1'b1;
					if(voltages_counter[2:0] == 3'b000) begin
						// store the two bytes into a buffer
						
						voltages_in_dat_pin0 <= next_voltage_pin0;
						voltages_in_dat_pin1 <= next_voltage_pin1;
						
						voltages_addr_pin0 <= voltages_counter[10:3];
						voltages_addr_pin1 <= voltages_counter[10:3];
						voltages_we <= 1'b1;
					end else if (voltages_counter[2:0] == 3'b010) begin
						// lower WE
						voltages_we <= 1'b0;
					end
					// shovel 8 bits at a time into each voltages entry
					next_voltage_pin0 <= {next_voltage_pin0[6:0], voltage_read_pin0};
					next_voltage_pin1 <= {next_voltage_pin1[6:0], voltage_read_pin1};
				end
			end
			
			if (polling_ctr < polling_frequency) begin
				polling_ctr <= polling_ctr + 1;
			end else begin
				polling_ctr <= 0;
			end
		end
		
	end
  
  reg [1:0]ppu_wr_st;
  reg [1:0]ppu_wr_m2_st;
  reg [5:0]ppu_wr_m2_skew_count;
  reg [5:0]ppu_wr_m2_skew_count_prev;
  reg [7:0]hasnt_changed_count;
  reg [4:0]alignment_skew_read_state;
  reg [23:0]alignment_sum;

  // regs for accessing alignment sample data in block ram
  reg [9:0]alignment_addr;
  reg alignment_we;
  reg [7:0]alignment_din;
  wire [7:0]alignment_dout;
  reg [9:0]current_alignment_sample;
  reg enough_written;
  // Alignment reader
  
  always @(negedge new_clk_100) begin
    if(mai.map_rst) begin
      ppu_wr_st <= 0;
      ppu_wr_m2_st <= 0;
      ppu_wr_m2_skew_count[5:0] <= 0;
      ppu_wr_m2_skew_count_prev[5:0] <= 0;
      hasnt_changed_count <= 0;
      alignment_skew_read_state <= 0;
      alignment_sum <= 0;
      alignment_addr <= 0;
      alignment_we <= 0;
      alignment_din <= 0;
      current_alignment_sample <= 0;
      enough_written <= 0;
    end else begin
      // watch for ppu /wr to go low
      ppu_wr_st[1:0] <= {ppu_wr_st[0], ppu.we};
      ppu_wr_m2_st[1:0] <= {ppu_wr_m2_st[0], cpu.m2};

      case(alignment_skew_read_state)
        0: begin
          // Wait for ppu wr to go low
          if(ppu_wr_st[1:0] == 2'b10) begin
            alignment_skew_read_state <= 1;
            ppu_wr_m2_skew_count[5:0] <= 0;
          end
        end
        1: begin
          // Wait for m2 to go high
          if(ppu_wr_m2_st[1:0] == 2'b01) begin
            // If it hasn't changed by only 1 in the last 100 reads, just give up and put the new value
            // Otherwise, only add values that are different from the most recent one by at most 1.
            if(hasnt_changed_count > 100) begin
              alignment_skew_read_state <= 2;
              hasnt_changed_count <= 0;
            end else if(ppu_wr_m2_skew_count_prev == ppu_wr_m2_skew_count) begin
              alignment_skew_read_state <= 2;
              hasnt_changed_count <= 0;
            end else if(ppu_wr_m2_skew_count_prev - 1 == ppu_wr_m2_skew_count) begin
              alignment_skew_read_state <= 2;
              hasnt_changed_count <= 0;
            end else if(ppu_wr_m2_skew_count_prev + 1 == ppu_wr_m2_skew_count) begin
              alignment_skew_read_state <= 2;
              hasnt_changed_count <= 0;
            end else begin
              alignment_skew_read_state <= 0;
              hasnt_changed_count <= hasnt_changed_count + 1;
            end
          end else begin
            ppu_wr_m2_skew_count[5:0] <= ppu_wr_m2_skew_count[5:0] + 1'b1;
          end
        end
        2: begin
          
          ppu_wr_m2_skew_count_prev[5:0] <= ppu_wr_m2_skew_count[5:0];
          
          // Subtract old sample value from total unless we haven't worked enough yet
          
          if(!enough_written) begin
            alignment_addr <= current_alignment_sample;
            alignment_skew_read_state <= 8;
          end else begin
            alignment_addr <= current_alignment_sample;
            
            alignment_skew_read_state <= 3;
          end
        end
        3: begin
          // wait state for getting old value
          
          alignment_skew_read_state <= 4;
        end
        4: begin
          // wait state for getting old value
          
          alignment_skew_read_state <= 5;
        end
        5: begin
          // wait state for getting old value
          
          alignment_skew_read_state <= 6;
        end
        6: begin
          // Data available from block ram
          alignment_sum[23:0] <= alignment_sum[23:0] - alignment_dout[7:0];
          
          alignment_skew_read_state <= 7;
        end
        7: begin

          alignment_skew_read_state <= 8;
        end
        8: begin
          // 18.62433 ~= 10010.10011111_11010100
          alignment_sum[23:0] <= alignment_sum[23:0] + ppu_wr_m2_skew_count[5:0];
          alignment_skew_read_state <= 9;
        end
        9: begin

          alignment_skew_read_state <= 10;
        end
        10: begin
          // Write new value to block ram for later subtraction
          alignment_din[7:0] <= {2'b00, ppu_wr_m2_skew_count[5:0]};
          alignment_we <= 1'b1;
          alignment_skew_read_state <= 11;
        end
        11: begin

          alignment_skew_read_state <= 12;
        end
        12: begin
          alignment_we <= 1'b0;

          alignment_skew_read_state <= 13;
        end
        13: begin

          alignment_skew_read_state <= 14;
        end
        14: begin
          current_alignment_sample[9:0] <= current_alignment_sample[9:0] + 1'b1;


          alignment_skew_read_state <= 15;
        end
        15: begin

          alignment_skew_read_state <= 16;
        end
        16: begin

          if(current_alignment_sample == 0) begin
            enough_written <= 1'b1;
          end
          alignment_skew_read_state <= 0;
        end
      endcase

    end
  end

endmodule

module ram_dp_old
(din_a, addr_a, we_a, dout_a, clk_a, din_b, addr_b, we_b, dout_b, clk_b);

	input [7:0]din_a, din_b;
	input [14:0]addr_a, addr_b;
	input we_a, we_b, clk_a, clk_b;
	output reg [7:0]dout_a, dout_b;

	
	reg [7:0]ram[17408];
	
	always @(negedge clk_a)
	begin
		dout_a <= we_a ? din_a : ram[addr_a];
		if(we_a)ram[addr_a] <= din_a;
	end
	
	always @(negedge clk_b)
	begin
		dout_b <= we_b ? din_b : ram[addr_b];
		if(we_b)ram[addr_b] <= din_b;
	end
	
endmodule