

module map_hub(
	input  MapIn mai,
	output MapOut mao
);

	assign mao = 	map_out_000_s8;

	MapOut map_out_000_s8;
	map_000_s8 m000_s8(mai, map_out_000_s8);
	
endmodule
