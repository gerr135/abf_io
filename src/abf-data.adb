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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada;
with Ada.Float_Text_IO; with Ada.Integer_Text_IO;

package body ABF.Data is

procedure WriteAtf (FileName : in String; data : in File_Contents) is separate;

--  This proc is a common part of both WriteAbf's.
--  No header adjustment is performed, only writing..
procedure WriteAbf_Common(FileName : in String; data : in File_Contents; abfH : in ABFFileHeader) is

	procedure Write_Header(File_Access : in Stream_IO.Stream_Access; abfH : ABFFileHeader) is
	  begin
		ABFFileHeader'Write(File_Access, abfH);
		--  is there anything else really?
	  end Write_Header;

	--  writes data array at current position in stream
	--  !!seek appropriately before calling this proc!!
	procedure Write_Data(File_Access : in Stream_IO.Stream_Access; data : DataRec) is

		--  a miltiplexed array that is going to be written directly to disk
		--  and we only write out IEEE floats (32 bits),
		--  so as not to loose any precision due to multiple conversions
		mplxData : MultiplexedSweeps_Float32(
				1 .. data.numSweeps,
				1 .. data.SegmentLen,
				1 .. data.numChannels);
	  begin
		--  so, we need to multiplex data here
		case data.acqMode is
		  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
			Channels:
			for nChn in data.chn'Range loop
				Sweeps:
				for nSwp in data.chn(nChn).swp'Range loop
					for nPnt in 1 .. data.SegmentLen loop
						mplxData(nSwp, nPnt, nChn) := data.chn(nChn).swp(nSwp)(nPnt);
					end loop;
				end loop Sweeps;
			end loop Channels;

		  when GapFree =>
			for nChn in data.channel'Range loop
				for nPnt in 1 .. data.SegmentLen loop
					mplxData(1, nPnt, nChn) := data.channel(nChn)(nPnt);
				end loop;
			end loop;

		  when VarLengthEvents =>
			raise Not_Implemented;
		end case;
		--  and write it out
		MultiplexedSweeps_Float32'Write(File_Access, mplxData);
	  end Write_Data;

	File : Stream_IO.File_Type;
	File_Access : Stream_IO.Stream_Access;

  begin  -- WriteAbf_Common
	Stream_IO.Create(File, Mode=>Stream_IO.Out_File, Name => FileName);
	File_Access := Stream_IO.Stream(File => File);

	Write_Header(File_Access, abfH);
	--  we write no sync array or other stuff here,
	--  so no need to seek, put data right after the header..
	Write_Data  (File_Access, data.data);

	Stream_IO.Close(File);
  end WriteAbf_Common;


procedure WriteAbf (FileName : in String; data : in File_Contents) is

	function Construct_Header(data : File_Contents) return ABFFileHeader is
		abfH : ABFFileHeader := Empty_GapFree_ABFFileHeader;
	  begin
		--  do necessary adjustments, following the ABF_UpdateHeader in Axon's abffiles.cpp
		raise Not_Implemented;
		return abfH;
	  end Construct_Header;

	abfH : ABFFileHeader := Construct_Header(data);

  begin
	WriteAbf_Common(FileName, data, abfH);
  end WriteAbf;


procedure WriteAbf (FileName : in String; data : in File_Contents; abfH : in ABFFileHeader) is

	procedure Adjust_Header(data : in File_Contents; abfH : in out ABFFileHeader) is
	  begin
		--  first check if passed header matches our data
		if (data.acqMode /= AcquisitionMode'Val(abfH.nOperationMode - 1)) or
		   (data.dt /= TimeBase_mks(abfH.fADCSampleInterval) * TimeBase_mks(abfH.nADCNumChannels))
		   or (abfH.fADCSecondSampleInterval /= 0.0) or
		   (data.numChannels /= ADC_ChannelIndex(abfH.nADCNumChannels)) or
		   (data.numSweeps   /= SweepIndex(abfH.lActualEpisodes))
		then
			raise Inconsistent_Header_Params;
		end if;

		--  now set params as necessary
		abfH.nDataFormat       := ABF_FLOATDATA;
		abfH.lDataSectionPtr   := (ABFFileHeader'Size/8) / ABF_BLOCKSIZE;
		abfH.nNumPointsIgnored := 0;
		case data.acqMode is
		  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
			abfH.lNumSamplesPerEpisode := C.long(data.SegmentLen * data.numChannels);
		  when GapFree =>
			abfH.lActualAcqLength := C.long(data.SegmentLen * data.numChannels);
		  when VarLengthEvents =>
			raise Not_Implemented;
		end case;

		--  looks like we are only left with copying the waveform over..
		PutWaveform (abfH, data.wf);
	  end Adjust_Header;

	newH : ABFFileHeader := abfH;

  begin  -- WriteAbf
	Adjust_Header(data, newH);
	WriteAbf_Common(FileName, data, newH);
  end WriteAbf;



function  Read_Data  (FileName : in String;  abfH : ABFFileHeader) return DataRec is
-- 	use Ada.Text_IO;
	dd : Data_Descriptors := GetDataDescriptors(abfH);
	data : DataRec(
		acqMode   => dd.opMode,   numChannels => dd.numChannels,
		numSweeps => dd.numSweeps, SegmentLen => dd.SegmentLen);

	File : Stream_IO.File_Type;
	File_Access : Stream_IO.Stream_Access;

  begin
	--  open and seek the stream,
	Stream_IO.Open(File, Mode=>Stream_IO.In_File, Name => FileName);
	File_Access := Stream_IO.Stream(File => File);
	Stream_IO.Set_Index(File, To => GetDataIndex(dd));

	case dd.DataFormat is
	  when Int16 =>
		declare
			--  define appropriate struct
			mplxData : MultiplexedSweeps_Int16(
				1 .. data.numSweeps,
				1 .. data.SegmentLen,
				1 .. data.numChannels);
			--  and conversion factors
			ADCtoUU : ADCtoUUFactors;
		begin
			--  read and demultiplex data
			MultiplexedSweeps_Int16'Read(File_Access, mplxData);
-- 			Put("<dbg> mplx size=");Integer_Text_IO.Put(mplxData'Size/8);New_Line;
			case data.acqMode is
			  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
				Channels16:
				for nChn in data.chn'Range loop
					ADCtoUU := GetADCtoUUFactors(abfH, nChn);
					Sweeps16:
					for nSwp in data.chn(nChn).swp'Range loop
						for nPnt in 1 .. data.SegmentLen loop
							data.chn(nChn).swp(nSwp)(nPnt) :=
								ADCtoUU.scale * mplxData(nSwp, nPnt, nChn)
								+ ADCtoUU.shift;
						end loop;
					end loop Sweeps16;
				end loop Channels16;

			  when GapFree =>
				for nChn in data.channel'Range loop
					ADCtoUU := GetADCtoUUFactors(abfH, nChn);
					for nPnt in 1 .. data.SegmentLen loop
						data.channel(nChn)(nPnt) :=
							ADCtoUU.scale * mplxData(1, nPnt, nChn) + ADCtoUU.shift;
					end loop;
				end loop;

			  when VarLengthEvents =>
				raise Not_Implemented;
			end case;
		end;

	  when Float32 =>
		declare
			--  define appropriate struct
			--  no need for conversion factors this time
			mplxData : MultiplexedSweeps_Float32(
				1 .. data.numSweeps,
				1 .. data.SegmentLen,
				1 .. data.numChannels);
		begin
			--  read and demultiplex data
			MultiplexedSweeps_Float32'Read(File_Access, mplxData);
			case data.acqMode is
			  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
				Channels32:
				for nChn in data.chn'Range loop
					Sweeps32:
					for nSwp in data.chn(nChn).swp'Range loop
						for nPnt in 1 .. data.SegmentLen loop
							data.chn(nChn).swp(nSwp)(nPnt) := mplxData(nSwp, nPnt, nChn);
						end loop;
					end loop Sweeps32;
				end loop Channels32;

			  when GapFree =>
				for nChn in data.channel'Range loop
					for nPnt in 1 .. data.SegmentLen loop
						data.channel(nChn)(nPnt) := mplxData(1, nPnt, nChn);
					end loop;
				end loop;

			  when VarLengthEvents =>
				raise Not_Implemented;
			end case;
		end;
	end case;

	Stream_IO.Close(File);
	return data;
  end Read_Data;


function ReadABFFile (FileName : in String) return File_Contents is
		abfH : ABFFileHeader := Read_Header(FileName);
		dd : Data_Descriptors := GetDataDescriptors(abfH);
		md : Mode_Descriptors := GetModeDescriptors(abfH);

		abfFile : File_Contents(
			acqMode => dd.opMode, numChannels => dd.numChannels,
			numSweeps => dd.numSweeps, SegmentLen => dd.SegmentLen);
  begin
	abfFile.dt := md.SampleInterval; -- SampleInterval

	--  comments, names and units
	for i in abfFile.comment'Range loop
		abfFile.comment(i) := Character(abfH.sFileComment(C.size_t(i-1)));
	end loop;

	for chn in 1 .. abfFile.numChannels loop
		for i in 1 .. ABF_ADCNAMELEN loop
			abfFile.channelName(chn)(i) := Character(abfH.sADCChannelName(
					PhysicalChannelNum(abfH, chn), C.size_t(i-1)));
		end loop;
		for i in 1 .. ABF_ADCUNITLEN loop
			abfFile.units(chn)(i) := Character(abfH.sADCUnits(
					PhysicalChannelNum(abfH, chn), C.size_t(i-1)));
		end loop;
	end loop;

	--  waveform and data
	if dd.opMode = EpisodicStimulation then
		abfFile.wf  := GetWafeform(abfH);
	end if;

	abfFile.data    := Read_Data(FileName, abfH);

	--  calculate signal ranges at ADC
	for nCh in 1 .. abfFile.numChannels loop
		declare
			factors : ADCtoUUFactors := GetADCtoUUFactors(abfH, nCh);
		begin
			abfFile.ADCMax(nCh) := (abfH.lADCResolution - 1) * factors.scale + factors.shift;
			abfFile.ADCMin(nCh) := (  - abfH.lADCResolution) * factors.scale + factors.shift;
		end;
	end loop; -- signal ranges at ADC

	return abfFile;
  end ReadABFFile;


procedure PrintData (data : File_Contents; File : in Text_IO.File_Type := Text_IO.Standard_Output) is
	use Ada.Integer_Text_IO; use Ada.Text_IO;
	use TimeBase_IO; use UserUnits_IO;
  begin
	New_Line(File);
	Put(File,"acquisition mode: ");Put_Line(File, data.acqMode'Img);
	Put(File, "N channels: ");Put(File, data.numChannels'Img);
	Put(File, ",  N sweeps: ");Put(File, data.numSweeps'Img);
	New_Line(File);
	Put(File, "length: ");Put(File, data.SegmentLen'Img);
	Put(File, ",  dt: ");Put(File, data.dt, Exp=>0, Aft=>1);
	New_Line(File);

	if data.acqMode = EpisodicStimulation then
		PrintWaveform (data.wf, File);
	end if;

	case data.acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		Put_Line(File, "data:");
		Put(File, "       t");
		for nChan in 1 .. data.numChannels loop Put(File, ",      ch#");Put(File, Integer(nChan), Width=>2); end loop;
		New_Line(File);
		Sweeps:
		for nSwp in 1 .. data.numSweeps loop
			Put(File, "sweep # "); Put(File, nSwp'Img);
-- 			if data.data.chn(1).swp(nSwp)=null then Put(File, ", null sweep!!"); end if;
			New_Line(File);
			Channels:
			for i in 1 .. data.SegmentLen loop
				--  time
				Put(File, (i-1)*data.dt, Fore=>6, Aft=>1, Exp=>0);
				--  and data
				for nChan in 1 .. data.numChannels loop
					Put(File, ", ");
					Put(File, data.data.chn(nChan).swp(nSwp)(i), Fore=>4, Aft=>5, Exp=>0);
				end loop;
				New_Line(File);
			end loop Channels;
		end loop Sweeps;

	  when GapFree =>
		Put_Line(File, "data:");
		Put(File, "       t");
		for nChan in 1 .. data.numChannels loop Put(File, ",      ch#");Put(File, Integer(nChan), Width=>2); end loop;
		New_Line(File);
		for i in 1 .. data.SegmentLen loop
			--  time
			Put(File, (i-1)*data.dt, Fore=>6, Aft=>1, Exp=>0);
			--  and data
			for nChan in 1 .. data.numChannels loop
				Put(File, ", ");
				Put(File, data.data.channel(nChan)(i), Fore=>4, Aft=>5, Exp=>0);
			end loop;
			New_Line(File);
		end loop;

	  when VarLengthEvents =>
		raise Not_Implemented;
	end case;

  end PrintData;

procedure Print2Plot (data : File_Contents; File : in Text_IO.File_Type := Text_IO.Standard_Output) is
	use Ada.Integer_Text_IO; use Ada.Text_IO;
	use TimeBase_IO; use UserUnits_IO;
  begin
	case data.acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		Put(File, "       t");
		for nSwp in 1 .. data.numSweeps loop
			for nChan in 1 .. data.numChannels loop
				Put(File, ",    s");Put(File, Integer(nSwp), Width=>2);
				Put(File, " c");Put(File, Integer(nChan), Width=>2);
			end loop;
		end loop;
		New_Line(File);
		Lines:
		for i in 1 .. data.SegmentLen loop
			--  time
			Put(File, (i-1)*data.dt, Fore=>6, Aft=>1, Exp=>0);
			--  and data
			Sweeps:
			for nSwp in 1 .. data.numSweeps loop
				for nChan in 1 .. data.numChannels loop
					Put(File, ", ");
					Put(File, data.data.chn(nChan).swp(nSwp)(i), Fore=>4, Aft=>5, Exp=>0);
				end loop;
			end loop Sweeps;
			New_Line(File);
		end loop Lines;

	  when GapFree =>
		Put(File, "       t");
		for nChan in 1 .. data.numChannels loop Put(File, ",      ch#");Put(File, Integer(nChan), Width=>2); end loop;
		New_Line(File);
		for i in 1 .. data.SegmentLen loop
			--  time
			Put(File, (i-1)*data.dt, Fore=>6, Aft=>1, Exp=>0);
			--  and data
			for nChan in 1 .. data.numChannels loop
				Put(File, ", ");
				Put(File, data.data.channel(nChan)(i), Fore=>4, Aft=>5, Exp=>0);
			end loop;
			New_Line(File);
		end loop;

	  when VarLengthEvents =>
		raise Not_Implemented;
	end case;
  end Print2Plot;


procedure CropValue(data : in File_Contents; nChan : in ADC_ChannelIndex;
		value : in out ADC_UserUnits) is
  begin
	if value > data.ADCMax(nChan) then value := data.ADCMax(nChan); end if;
	if value < data.ADCMin(nChan) then value := data.ADCMin(nChan); end if;
  end CropValue;

function  CropValue(data : File_Contents; nChan : ADC_ChannelIndex;
		value : ADC_UserUnits) return ADC_UserUnits is
	newVal : ADC_UserUnits := value;
  begin
	CropValue(data, nChan, newVal);
	return newVal;
  end CropValue;

---------------------------------------------------------------------------

function GetDataDescriptors(abfH : ABFFileHeader) return Data_Descriptors is
	md : Mode_Descriptors := GetModeDescriptors(abfH);
	dataDescr : Data_Descriptors;
  begin
  	dataDescr.opMode       := md.acqMode;
	dataDescr.numChannels  := ADC_ChannelIndex(abfH.nADCNumChannels);
	dataDescr.DataFormat   := ABFDataFormat'Val(abfH.nDataFormat);
	dataDescr.DataSectionPtr   := BlockCount(abfH.lDataSectionPtr);
	dataDescr.NumPointsIgnored := SampleCount(abfH.nNumPointsIgnored);
	case dataDescr.opMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		dataDescr.SegmentLen := SampleCount(abfH.lNumSamplesPerEpisode) / dataDescr.numChannels;
		dataDescr.numSweeps  := SweepIndex(abfH.lActualEpisodes);
	  when GapFree =>
		dataDescr.SegmentLen := SampleCount(abfH.lActualAcqLength) / dataDescr.numChannels;
		--  Axon seems to use lActualEpisodes from what is described!!
		--  force-set numSweeps in this mode
		dataDescr.numSweeps := 1;
	  when VarLengthEvents => raise Not_Implemented;
	end case;
	return dataDescr;
  end GetDataDescriptors;


function GetDataIndex(dd : Data_Descriptors) return Stream_IO.Count is
	use Ada.Text_IO; use Stream_IO;
	SampleSize : Stream_IO.Count := 0;
  begin
	case dd.DataFormat is
		when Int16   => SampleSize := C.short'Size;
		when Float32 => SampleSize := C.C_float'Size;
	end case;
	return Stream_IO.Count(dd.DataSectionPtr*ABF_BLOCKSIZE + 1)
			+ Stream_IO.Count(dd.NumPointsIgnored)*SampleSize;
  end GetDataIndex;


-----------------------------------------------------------------------------------------------

procedure Initialize (data : in out DataRec) is
-- 	use Ada.Text_IO;
  begin
-- 	data.trackingNum := globalTrack; globalTrack := globalTrack + 1;
-- 	Put("<dbg>Initialize(");Put(data.trackingNum'Img);Put("):  data.acqMode=");
-- 	Put(data.acqMode'Img);Put(",  nCh=");Put(data.numChannels'Img);
-- 	Put(",  nSW=");Put(data.numSweeps'Img);Put(",  len=");Put_Line(data.SegmentLen'Img);
	case data.acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		for i in 1 .. data.numChannels loop
			--  allocate channels
			data.chn(i) := new DataChn(
				numSweeps => data.numSweeps,
				SweepLen => data.SegmentLen);
-- 			Put("  chn[");Put(i'Img);Put("]=(ind:");Put(data.chn(i).numSweeps'Img);Put(", len:");
-- 			Put(data.chn(i).SweepLen'Img);Put_Line(")");
			-- allocate sweeps
			for j in 1 .. data.numSweeps loop
				data.chn(i).swp(j) := new DataSegment(1 .. data.SegmentLen);
-- 				Put("  swp[");Put(j'Img);Put("]=");Put_Line(data.chn(i).swp(j)'Last'Img);
			end loop;
		end loop;

	  when GapFree =>
		for i in 1 .. data.numChannels loop
			--  allocate channels
			data.channel(i) := new DataSegment (1 .. data.SegmentLen);
		end loop;

	  when VarLengthEvents => null;
	end case;
  end;

procedure Finalize   (data : in out DataRec) is
--  	use Ada.Text_IO;
	procedure Free is new
		Ada.Unchecked_Deallocation(DataChn, DataChnAccess);
	procedure Free is new
		Ada.Unchecked_Deallocation(DataSegment, DataSegmentAccess);
  begin
-- 	Put("<dbg>Finalize(");Put(data.trackingNum'Img);Put("):  data.acqMode=");
-- 	Put(data.acqMode'Img);Put(",  nCh=");Put(data.numChannels'Img);
-- 	Put(",  nSW=");Put(data.numSweeps'Img);Put(",  len=");Put_Line(data.SegmentLen'Img);
	case data.acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		for i in data.chn'Range loop
			-- first check if we are not finalized already
			if data.chn(i) /= null then
				--  I don't think we can ever get partially initialized array,
				--  but wtf, lets reclaim all storage anyway
				for j in data.chn(i).swp'Range loop
					Free(data.chn(i).swp(j));
				end loop;
			end if;
			Free(data.chn(i));
		end loop;

	  when GapFree =>
		for i in data.channel'Range loop
			Free(data.channel(i));
		end loop;

	  when VarLengthEvents => null;
	end case;
--	Put_Line("<dbg>Finalize done");
  end;

procedure  Adjust(data : in out DataRec) is
-- 	use Ada.Text_IO;
-- 	tmpChnPtr : DataChnAccess;
-- 	tmpSegPtr : DataSegmentAccess;
  begin
-- 	Put("<dbg>Adjust(");Put(data.trackingNum'Img);Put("):  data.acqMode=");
-- 	Put(data.acqMode'Img);Put(",  nCh=");Put(data.numChannels'Img);
-- 	Put(",  nSW=");Put(data.numSweeps'Img);Put(",  len=");Put_Line(data.SegmentLen'Img);

	case data.acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		if data.chn'Last /= data.numChannels then
			raise Inconsistent_DataRecord;
		end if;

		for i in data.chn'Range loop
			-- exit when data.chn(i) = null;
				--  we might be passed a non-initialized record, nothing to copy
			if data.chn(i).swp'Last /= data.numSweeps then
				raise Inconsistent_DataRecord;
			end if;
			--  keep old pointer and allocate channels anew
-- 			tmpChnPtr := data.chn(i);
			data.chn(i) := new DataChn'(data.chn(i).all);
-- 				numSweeps => tmpChnPtr.numSweeps,
-- 				SweepLen  => tmpChnPtr.SweepLen);

			-- allocate and copy sweeps
			for j in data.chn(i).swp'Range loop
-- 				tmpSegPtr := data.chn(i).swp(j);
-- 				if tmpSegPtr = null then
-- 					Put("<dbg> null segment!! chn=");Put(i'Img);Put(", swp=");Put_Line(j'Img);
-- 				end if;
-- 				exit when tmpSegPtr = null;
				data.chn(i).swp(j) := new DataSegment'(data.chn(i).swp(j).all);
			end loop;
		end loop;

	  when GapFree =>
		if data.channel'Last /= data.numChannels then
			raise Inconsistent_DataRecord;
		end if;

		for i in data.channel'Range loop
			exit when data.channel(i) = null;
-- 			tmpSegPtr := data.channel(i);
			data.channel(i) := new DataSegment'(data.channel(i).all);
		end loop;

	  when VarLengthEvents => null;
	end case;
-- 	Put_Line("<dbg>Adjust done");
  end;


end ABF.Data;
