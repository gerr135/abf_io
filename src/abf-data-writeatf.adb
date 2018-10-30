--**************************************************************************
--   Copyright (C) 2005 by George Shapovalov  --
--   george@gentoo.org  --
--                                                                        --
--   This program is free software; you can redistribute it and/or modify --
--   it under the terms of the GNU Library General Public License as      --
--   published by the Free Software Foundation; either version 2 of the   --
--   License, or (at your option) any later version.                      --
--                                                                        --
--   This program is distributed in the hope that it will be useful,      --
--   but WITHOUT ANY WARRANTY; without even the implied warranty of       --
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        --
--   GNU General Public License for more details.                         --
--                                                                        --
--   You should have received a copy of the GNU Library General Public    --
--   License along with this program; if not, write to the                --
--   Free Software Foundation, Inc.,                                      --
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.            --
--**************************************************************************

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed; with Ada.Strings.Bounded;

separate (ABF.Data)
procedure WriteAtf (FileName : in String; data : in File_Contents) is

	File : Text_IO.File_Type;

	procedure Write_Header(File : in out Text_IO.File_Type; data : in File_Contents) is
		use Ada.Characters; use Ada.Integer_Text_IO;
		use Ada.Strings; use Ada.Strings.Fixed;
		use TimeBase_IO;

-- 		package acqModeStrings is new Ada.Strings.Bounded.Generic_Bounded_Length
-- 			(Max => "Variable-Length Event-Driven"'Length);
-- 		use acqModeStrings;
		
		AcqModeNames : array(AcquisitionMode) of String(1 .. 28) :=
			(VarLengthEvents     => "Variable-Length Event-Driven",
			 FixedLengthEvents   => "Fixed-Length Event-Driven   ",
			 GapFree             => "Gap-Free                    ",
			 Oscilloscope        => "High-Speed Oscilloscope     ",
			 EpisodicStimulation => "Episodic Stimulation        ");

	begin
		--  ATF header
		Put_Line(File, "ATF" & Latin_1.HT & "1.0");
		--  field numerator, n comments <tab> n entries = nSweeps*NChannels+1
		Put(File, "4" & Latin_1.HT);
		Put(File, Integer(data.numSweeps)*Integer(data.numChannels)+1, Width=>1);
		New_Line(File);
		--  some comments, and we need to pass "'s, so bear with extra quotes..
		Put(File, """AcquisitionMode=");
		Put(File, Trim(AcqModeNames(data.acqMode), Side=>Both));
		Put_Line(File, """");
		Put_Line(File, """Comment=" & Trim(data.comment, Side=>Both) & """");
		Put(File, """SyncTimeUnits=");Put(File, data.dt, Fore=>1, Aft=>0, Exp=>0);
			Put_Line(File, """");

		Put(File, """Signals=""");
		for swp in 1 .. data.numSweeps loop
			for chn in 1 .. data.numChannels loop
				Put(File, Latin_1.HT & """" & Trim(data.channelName(chn), Side=>Both) & """");
			end loop;
		end loop;
		New_Line(File);

		--  now, finally, the units
		Put(File, """Time (s)""");
		for swp in 1 .. data.numSweeps loop
			for chn in 1 .. data.numChannels loop
				Put(File, Latin_1.HT & """" & Trim(data.units(chn), Side=>Both) & """");
			end loop;
		end loop;
		New_Line(File);
	end;

	procedure Write_Data  (File : in out Text_IO.File_Type; data : in File_Contents) is
		use TimeBase_IO; use UserUnits_IO;
		use Ada.Characters;

	begin
		case data.acqMode is
		  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
			--  the data is 1-line per multiplexed sample set - all sweeps and channels
			--  format is:
			--  time<tab>swp1Chn1<tab>swp1Chn2<tab>...swp'LastChn'Last
			SampleCount_Episodic:
			for i in 1 .. data.SegmentLen loop
				--  time is in seconds here
				Put(File, (i-1)*data.dt/1000_000.0, Fore=>1, Aft=>4, Exp=>1);
				Sweeps_Episodic:
				for nSwp in 1 .. data.numSweeps loop
					for nChan in 1 .. data.numChannels loop
						Put(File, Latin_1.HT);
						Put(File, data.data.chn(nChan).swp(nSwp)(i), Fore=>1, Aft=>6, Exp=>0);
					end loop;
				end loop Sweeps_Episodic;
				New_Line(File);
			end loop SampleCount_Episodic;

		  when GapFree =>
			SampleCount_GapFree:
			for i in 1 .. data.SegmentLen loop
				Put(File, (i-1)*data.dt/1000_000.0, Fore=>1, Aft=>4, Exp=>1);
				for nChan in 1 .. data.numChannels loop
					Put(File, Latin_1.HT);
					Put(File, data.data.channel(nChan)(i), Fore=>1, Aft=>6, Exp=>0);
				end loop;
				New_Line(File);
			end loop SampleCount_GapFree;

		  when VarLengthEvents =>
			raise Not_Implemented;
		end case;
	end Write_Data;

begin
	Create(File, Mode => Out_File, Name => FileName);
	Write_Header(File,data);
	Write_Data  (File,data);
	Close (File);
end WriteAtf;
