--
--
--  Ada Spec: abf-data
--
--  Description: Actual data handling
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2005
--
--  Copyright: See COPYING file that comes with this distribution
--
--


with ABF.Header; use ABF.Header;
with ABF.Header.Waveform; use ABF.Header.Waveform;
with C;

with Ada.Finalization; use Ada.Finalization;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO; use Ada;

package ABF.Data is

	Inconsistent_DataRecord : exception;

	--  An end-point data segment,
	--  "sweep" in EpisodicStimulation mode and "channel" in GapFree mode
	type DataSegment is array (SampleIndex range <>) of ADC_UserUnits;
	type DataSegmentAccess is access all DataSegment;
	--  but we better use separate array types for index range checking
	type DataSweepArray   is array (SweepIndex range <>) of DataSegmentAccess;
	type DataChannelArray is array (ADC_ChannelIndex range <>) of DataSegmentAccess;
		--  see remark in DataRec definition wrt naming

	type DataChn (numSweeps : SweepIndex := 1; SweepLen : SampleCount := 0) is record
		swp : DataSweepArray (1 .. numSweeps);
	end record;
	type DataChnAccess is access all DataChn;
	type DataChnArray  is array (ADC_ChannelIndex range <>) of DataChnAccess;
		--  see remark in DataRec definition wrt naming


	---------  The actual record -----------------------------
	---
	---  NOTE!   NOTE!   NOTE!   NOTE!   NOTE!   NOTE!  --
	---  making DataRec controlled simplifies its handling but is not optimal in the case
	---  of large datasets, as this causes many calls to Adjust/Finalize,
	--   unnecessarily moving data around.
	--   This may have to be revisited  ----------------------
	type DataRec (
		acqMode     : AcquisitionMode;
		numChannels : ADC_ChannelIndex;
		numSweeps   : SweepIndex;
		SegmentLen  : SampleCount -- num samples for either sweep or channel, depending on acqMode
			) is new Controlled with record
		trackingNum : Natural := 0;
	case acqMode is
	  when FixedLengthEvents | Oscilloscope | EpisodicStimulation =>
		chn     : DataChnArray (1 .. numChannels);
		--  field name has to be distinct and it makes sense to abbreviate in this case,
		--  as we will have nested chn(i).swp(j) throughout
	  when GapFree =>
		channel : DataChannelArray (1 .. numChannels);
	  when VarLengthEvents =>
		null;  -- not supported yet
	end case; end record;

	procedure Initialize (data : in out DataRec);
	procedure Adjust     (data : in out DataRec);
	procedure Finalize   (data : in out DataRec);

--	procedure Write_Data (data : in DataRec; FileName : String);
	function  Read_Data  (FileName : in String; abfH : ABFFileHeader) return DataRec;


	-----------------------------------------------------------------------------
	--  You will probably want to use this structure/methods    -----------------
	-----------------------------------------------------------------------------
	type ChannelNames is array (ADC_ChannelIndex range <>) of String(1 .. ABF_ADCNAMELEN);
	type ChannelUnits is array (ADC_ChannelIndex range <>) of String(1 .. ABF_ADCUNITLEN);
	type SignalValues is array (ADC_ChannelIndex range <>) of ADC_UserUnits;

	type File_Contents (
		acqMode : AcquisitionMode := GapFree;
		numChannels : ADC_ChannelIndex := 1;
		numSweeps   : SweepIndex := 1;
		SegmentLen  : SampleCount := 0
			) is

	record

		dt      : TimeBase_mks := 0.0; -- demultiplexed SampleInterval
			--  SecondSampleInterval not supported yet

		comment : String(1 .. ABF_FILECOMMENTLEN) := (others=>' ');
		channelName : ChannelNames(1 .. numChannels);
		units       : ChannelUnits(1 .. numChannels);

		data    : DataRec(acqMode, numChannels, numSweeps, SegmentLen);
			--  dataRec is just one field, but has some associated code
			--  so avoid duplication, use whole record

		ADCMin, ADCMax : SignalValues(1 .. numChannels);
			--  range of signal at ADC (as provided by amplifier)
			--  used for clipping during data manipulation

		case acqMode is
		  when EpisodicStimulation =>
			wf      : WaveformRec;
		  when others =>
			null;
		end case;
	end record;

	function  ReadABFFile (FileName : String) return File_Contents;

	procedure WriteAtf (FileName : in String; data : in File_Contents);
	procedure WriteAbf (FileName : in String; data : in File_Contents);
	procedure WriteAbf (FileName : in String; data : in File_Contents; abfH : in ABFFileHeader);
		--  1st WriteAbf constructs header anew, use it at your own risk, when you lost original header
		--  2nd adjusts the passed header to resemble new sizes, etc. and then uses it with the data..
		--  *only* the relevant fields are updated, so you may pass some bogus header, but no garanties then..

	procedure PrintData (data : File_Contents; File : in Text_IO.File_Type := Text_IO.Standard_Output);
	procedure Print2Plot(data : File_Contents; File : in Text_IO.File_Type := Text_IO.Standard_Output);
		--  a-la atf, simply print stuff out for verification

	function  CropValue(data : File_Contents; nChan : ADC_ChannelIndex;
		value : ADC_UserUnits) return ADC_UserUnits;
	procedure CropValue(data : in File_Contents; nChan : in ADC_ChannelIndex;
		value : in out ADC_UserUnits);

private

	----------  Some header fields specific to the data ----
	----------  a semi-thin binding - just repeat the relevant fields in native types
	type Data_Descriptors is record
		opMode           : AcquisitionMode := GapFree;
		numChannels      : ADC_ChannelIndex := 1;
			-- nADCNumChannels
		numSweeps        : SweepIndex := 1;
			--  there is a bit of field duplication with this field repeated here
			--  (it is also in the waveform)
			--  but this allows to keep all the relevant data together

		--  data format
		DataFormat       : ABFDataFormat := Int16;
		DataSectionPtr   : BlockCount := 0;
			--  seems easier to keep in blocks - may use Direct_IO then
		NumPointsIgnored : SampleCount := 0;
-- 		SecondsPerRun        : TimeBase_sec := 0.0; -- may use, but no need now
-- 			--  requested acq length in sec.
-- 			--  0  = available disk space
-- 			--  -1 = ignore this, use EpisodesPerRun

		SegmentLen       : SampleCount := 0;
	end record;

	function GetDataDescriptors(abfH : ABFFileHeader) return Data_Descriptors;

	function GetDataIndex(dd : Data_Descriptors) return Stream_IO.Count;
		--  returns index into the stream at which to seek to data start
		--  in bytes really, as these seem to be "stream elements"
		--  anyway these should be the same as "storage elements" in which all 'Size(es) are given..


	-------------------------------------------------------------------------
	--  the binary structure of Data Section of ABF file
	-------------------------------------------------------------------------

	--  single sample read - multiplexed over acquired channels
	type Multiplexed_Sample_Int16   is array(ADC_ChannelIndex range <>) of C.short;
	type Multiplexed_Sample_Float32 is array(ADC_ChannelIndex range <>) of ADC_UserUnits;
	pragma Convention (C, Multiplexed_Sample_Int16);
	pragma Convention (C, Multiplexed_Sample_Float32);

	--  data segment - either single wseep or gapfree recording
	--  has to be 2D array, as
	type MultiplexedSegment_Int16   is array(SampleIndex range <>, ADC_ChannelIndex range <>) of C.short;
	type MultiplexedSegment_Float32 is array(SampleIndex range <>, ADC_ChannelIndex range <>) of ADC_UserUnits;
	pragma Convention (C, MultiplexedSegment_Int16);
	pragma Convention (C, MultiplexedSegment_Float32);

	--  array of sweeps -
	type MultiplexedSweeps_Int16   is array(
		SweepIndex  range <>,
		SampleIndex range <>,
		ADC_ChannelIndex range <>) of C.short;
	type MultiplexedSweeps_Float32 is array(
		SweepIndex  range <>,
		SampleIndex range <>,
		ADC_ChannelIndex range <>) of ADC_UserUnits;
	pragma Convention (C, MultiplexedSweeps_Int16);
	pragma Convention (C, MultiplexedSweeps_Float32);

	-- filed for tracking Initialize/Finalize for DataRec
	--was complemented by field trackingNum in dataRec
	globalTrack : Positive := 1;

end ABF.Data;