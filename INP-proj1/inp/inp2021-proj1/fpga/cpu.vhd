-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): xkuzni04
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

 -- Sinnaly
 --****************************************
 --****************************************
 -- Prgoram counter -------------------
		signal pc_rom_addres 	: std_logic_vector (11 downto 0);
		signal pc_inc     		: std_logic;
		signal pc_dec     		: std_logic;
		signal pc_clear			: std_logic;
		--signal pc_load  			: std_logic;
 --------------------------------------

  -- PTR to RAM  -------------------
		signal ptr_ram_addres 	 : std_logic_vector (9 downto 0);
		signal ptr_inc     	    : std_logic;
		signal ptr_dec     		 : std_logic;
		signal ptr_clear			 : std_logic;
 --------------------------------------
 
   -- RAM  -------------------
		--signal ram_send 	  : std_logic_vector (11 downto 0);
		signal ram_push     : std_logic;
		signal ram_pop      : std_logic;
		
		
		signal mx_switch : std_logic_vector (1 downto 0) := (others => '0');
		signal mx_out 	  : std_logic_vector (7 downto 0) := (others => '0');
		
 --****************************************
 --****************************************
 
 
 
 --- STATES -------------------------
 --****************************************
		type fsm_state is (
		
			s_start, 				  -- starts program
			s_load_instruction,    -- lOADS NEXT INSTRUCTION
			s_decode,				  -- Parse next command 
			
			s_command_ptr_inc, 	  -- >
 			s_command_ptr_dec,	  -- <
			
			s_command_val_inc_1,	  -- +
			s_command_val_inc_2,	  -- +
			s_command_val_inc_3,   -- +

			s_command_val_dec_1,	  -- -
			s_command_val_dec_2,	  -- -
			s_command_val_dec_3,	  -- -

			
			s_command_putchar_1,   -- .
			s_command_putchar_2,   -- .
			s_command_putchar_3,

			s_command_getchar_1,   -- ,
			s_command_getchar_2,   -- ,
			s_command_getchar_3,   -- ,
			
			
			s_command_return,
			s_not_command,
			
			
			
			
			s_command_while_start, -- [
			s_command_while_end,   -- ]
			s_command_break,  	  -- ~
			s_command_while_check,
			s_command_while_loop,
			s_command_while_en,
			
			
			
			s_command_null -- null
			
			);
			signal state : fsm_state := s_start;
			signal next_state : fsm_state;
--****************************************

begin

--########## PTR ###################################################
		ptr: process (CLK, RESET, ptr_inc, ptr_dec, ptr_clear) is 
		begin 	
			if RESET = '1' then
				ptr_ram_addres <= ( others => '0'); -- Set addres to zeros
			elsif rising_edge(CLK) then
				if ptr_inc = '1' then  	 -- > pointer increment 
					ptr_ram_addres <= ptr_ram_addres + 1;
				elsif ptr_dec = '1' then -- < ptr decrement
					ptr_ram_addres <= ptr_ram_addres - 1;
				elsif ptr_clear = '1' then
					ptr_ram_addres <= "0000000000";
				end if;			
			end if;
		end process;
		DATA_ADDR <= ptr_ram_addres;
		--DATA_ADDR <= "0000" & ireg_reg(11 downto 0) when mx_switch=""
--#####################################################################

--########## PROGRAM COUNTER ###################################################
		pc: process (CLK, RESET, pc_inc, pc_dec, pc_clear) is 
		begin 
				if RESET = '1' then
						pc_rom_addres <= (others => '0');
				elsif rising_edge(CLK) then
					if pc_inc 		= '1' then
						pc_rom_addres <= pc_rom_addres + 1;
					elsif pc_dec 	= '1' then
						pc_rom_addres <= pc_rom_addres -1;
					elsif pc_clear = '1' then
						pc_rom_addres <= "000000000000";
					end if;
				end if;
		end process;
		CODE_ADDR <= pc_rom_addres;
--#####################################################################

--########## MULTIPLEXOR   #############################################
		mux: process (CLK, RESET) is 
		begin 
				if RESET = '1' then
						mx_out <= (others => '0');
				elsif rising_edge(CLK) then
					case mx_switch is
						when "00" =>
							mx_out <= IN_DATA;
						when "01" =>
							mx_out <= DATA_RDATA;
						when "10" =>
							mx_out <= DATA_RDATA + 1; --hodnota aktualni bunkz
						when "11" =>
							mx_out <= DATA_RDATA - 1;
						when others =>
							null;
					end case;
				end if;
		end process;
		DATA_WDATA <= mx_out;
--#####################################################################


-- ########### FSM get state ####################################################
		state_logic: process (CLK, RESET, EN) is 
		begin
			if RESET = '1' then 
				state <= s_start;
			elsif rising_edge(CLK) then
				if EN = '1' then 
					state <= next_state;
				end if;
			end if;
		end process;
-- ########### FSM get state ####################################################

		
-- ########### FSM LOGIC #############################################
		fsm: process (state, OUT_BUSY, IN_VLD, CODE_DATA, DATA_RDATA) is
		begin
			---inicializace 
			-- PC
			pc_inc 	<= '0';
			pc_dec 	<= '0';
			pc_clear <= '0';
			ram_pop 	<= '0';
			ram_push <= '0';
				--pc_ld 	<= '0';
			-- PTR 
			ptr_inc 	 <= '0';
			ptr_dec 	 <= '0';
			ptr_clear <= '0';
			-- I/O
			--OUT_DATA <= "00000000";
			IN_REQ 	<= '0';
			-- ROM 
			CODE_EN 		<= '0';
			OUT_WREN  	<= '0';
			-- RAM
			DATA_EN 		<= '0';
			DATA_WREN 	<= '0';
			-- MUX
			mx_switch 	<= "00";
			--mx_out 		<= "00000000";
			
						
			-- DEFINUJU CO SE MA DELAT PODLE PSEUDO KODU 
			case state is
			-- Vychozi stav - clear all registers
					when s_start =>
						ptr_clear <= '1';
						pc_clear  <= '1';
						--todo cnt_clear <= '1';
						next_state 	  <= s_load_instruction;
								
					when s_load_instruction =>
						CODE_EN <= '1';  
						next_state <= s_decode;
					
					
					when s_command_ptr_inc =>	  -- +
						ptr_inc    <= '1';
						pc_inc 	  <= '1';
						next_state <= s_load_instruction;
					
					
					when s_command_ptr_dec =>	  -- -
						ptr_dec    <= '1';
						pc_inc 	  <= '1';
						next_state <= s_load_instruction;
			
			
					when s_command_val_inc_1 => 	  -- >
						DATA_EN 		<= '1';
						DATA_WREN 	<= '0';
						next_state <= s_command_val_inc_2;
					when s_command_val_inc_2 => 	  -- >
						mx_switch   <= "10"; 
						next_state  <= s_command_val_inc_3;
					when s_command_val_inc_3 => 	  -- >
						DATA_EN 		<= '1';
						DATA_WREN   <= '1';
						pc_inc 		<= '1';
						next_state  <= s_load_instruction;
						
						
					when s_command_val_dec_1 => -- <
						DATA_EN 		<= '1';
						DATA_WREN 	<= '0';
						next_state <= s_command_val_dec_2;
					when s_command_val_dec_2 => -- <
						mx_switch   <= "11";
						next_state <= s_command_val_dec_3;
					when s_command_val_dec_3 => -- <
						DATA_EN 		<= '1';
						DATA_WREN 	<= '1';
						pc_inc 		<= '1';
						next_state <= s_load_instruction;



					when s_command_putchar_1 => -- ,
						if OUT_BUSY = '0' then
							DATA_EN <= '1';
							next_state <= s_command_putchar_2;
						else
							next_state <= s_command_putchar_1;
						end if;
					when s_command_putchar_2 => -- ,
						next_state <= s_command_putchar_3;
					when s_command_putchar_3 => -- ,
						OUT_DATA <= DATA_RDATA;
						OUT_WREN <= '1';
						pc_inc   <= '1';
						next_state <= s_load_instruction;
						
							
					when s_command_getchar_1 =>  -- ,
						IN_REQ 	  <= '1';
						mx_switch  <= "00";		  -- send input data to output 
						next_state <= s_command_getchar_2;		
					when s_command_getchar_2 =>
						if IN_VLD /= '1' then 
							IN_REQ <= '1';
							mx_switch  <= "00";		  -- send input data to output 
							next_state <= s_command_getchar_2;
						else
							next_state <= s_command_getchar_3;
						end if;
					when s_command_getchar_3 =>
						DATA_EN 	  <= '1';
						DATA_WREN  <= '1';
						pc_inc 	  <= '1';
						next_state <= s_load_instruction;
					
								
					when s_decode =>
						case CODE_DATA is
							when X"3E" => -- >
								next_state <= s_command_ptr_inc; -- ptr += 1;
							when X"3C" => -- <
								next_state <= s_command_ptr_dec; -- ptr -= 1;								
							when X"2B" => -- +
								next_state <= s_command_val_inc_1; -- *ptr += 1;
							when X"2D" => -- -
								next_state <= s_command_val_dec_1;	-- *ptr -= 1;	
	--						when X"5B" => -- [
	--							next_state <= s_command_while_start; -- while (*ptr) {
	--						when X"5D" => -- ]
	--							next_state <= s_command_while_end;	 -- }
							when X"2E" => -- .
								next_state <= s_command_putchar_1;     -- puchar(*ptr);
							when X"2C" => -- ,
								next_state <= s_command_getchar_1;		 -- *ptr = getchar();
	--						when X"7E" => -- ~
	--							next_state <= s_command_break;       	-- break
							when X"00" => -- null
								next_state <= s_command_return;        -- return;
							when others => 	-- if non of coomands go to next 
								next_state <= s_not_command;
						end case;	
						
						when s_command_return =>
							next_state <= s_command_return;

						-- if not command skip to next 
						when s_not_command =>
							pc_inc <= '1';
							next_state <= s_load_instruction;
						
						when others =>
							null; 			--  null -end of program 
				
				end case;		
		end process;
-- ##########################################################################################
	
				
	--					
	--			when s_command_while_start => -- [
	--				pc_inc <= '1';
	--				DATA_EN <= '1';
	--				DATA_WREN <= '0';
	--				next_state <= s_command_while_check;
	--			
	--			when s_command_while_check =>
	--				if DATA_RDATA /= "00000000" then
	--					next_state <= s_fetch;
	--				else
	--					CODE_EN <= '1';
	--					next_state <= s_command_while_loop;
	--				end if;
	--			
	--			when s_command_while_loop =>
	--				if CODE_DATA = X"5D" then 
	--					next_state <= s_fetch;
	--				else 
	--					next_state <= s_command_while_en;
	--				end if;
					----
	--			when s_command_while_en =>
	--				DATA_EN <= '1';
	--				next_state <= s_command_while_loop;
					
					
	--			when s_command_while_end =>   -- ]
	--				if DATA_RDATA = "00000000" then -- neni nul 
					--	pc_ld <= '1';
	--					next_state <= s_fetch;
	--				else 
	--					pc_inc <= '1';
	--					ram_pop <= '1';
	--					next_state <= s_fetch;
	--				end if;
	--			
								

end behavioral;
 
