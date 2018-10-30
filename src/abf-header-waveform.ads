--
--
--  Ada Spec: abf_waveform
--
--  Description: Waveform extraction and manipulation
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2005
--
--  Copyright: See COPYING file that comes with this distribution
--
--

with C;
with Ada.Text_IO; use Ada;

package ABF.Header.Waveform is

	------ Digital Outputs part -------------------
	type InterEpisodeHolding is (Holding, LastEpoch);
	type DigitalValueArray is array (EpochIndex) of C.short;
	type DigitalTrainStartPolarity is (Low, High);

	type Digital_Train is record
		Enabled             : Boolean  := False;
		ActiveDACChannel    : DAC_ChannelIndex := 1;
			--  not sure if this field is needed
			--  the abf description says redundant..
		DigitalHolding      : C.short  := 0;  -- holding bit pattern? why 2 byte?
		DigitalInterEpisode : InterEpisodeHolding := Holding;
		DigitalValue        : DigitalValueArray := (others => 0);
		--  from the Extended section
		DigitalTrainValue   : DigitalValueArray := (others => 0);
		DigitalTrainActiveLogic : DigitalTrainStartPolarity := Low;
	end record;


	function  GetDigiTrain (abfH : in Header.ABFFileHeader) return Digital_Train;
	procedure PutDigiTrain (abfH : in out Header.ABFFileHeader; dt : in Digital_Train);
	procedure PrintDigiTrain (
			dg : in Digital_Train;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True); -- if False print all (even empty) fields


	------ The waveform part -------------------------
	type wfEnabledArray is array (WaveformIndex) of Boolean;
	type wfSource   is (Disabled, FromEpochDefs, FromDAC_File);
	type wfSrcArray is array (WaveformIndex) of wfSource;
	type wfInterEpisodeHoldingArray is array (WaveformIndex) of InterEpisodeHolding;

	type EpochTypes is (Disabled, Step, Ramp);
	type EpochTypeArray   is array (EpochIndex) of EpochTypes;
		--  epoch order - same DAC
	type wfEpochTypes  is array (WaveformIndex) of EpochTypeArray;
		--  Waveform type - 2D array, in DAC order

	type EpochLevelArray is array (EpochIndex) of DAC_UserUnits;
	type wfEpochLevels   is array (WaveformIndex) of EpochLevelArray;

	type SampleCountArray is array (EpochIndex) of SampleCount;
	type wfSampleCount    is array (WaveformIndex) of SampleCountArray;

	type Waveform_Shapes is record
		--  Sweep-wide params
		DACEnabled        : wfEnabledArray  := (others => False);
		WaveformSource    : wfSrcArray  := (others => Disabled);
		InterEpisodeLevel : wfInterEpisodeHoldingArray
			:= (others => Holding);
		--  epoch definitions
		EpochType         : wfEpochTypes  := (others => (others => Disabled));
		EpochInitLevel    : wfEpochLevels := (others => (others => 0.0));
		EpochLevelInc     : wfEpochLevels := (others => (others => 0.0));
		EpochInitDuration : wfSampleCount := (others => (others => 0));
		EpochDurationInc  : wfSampleCount := (others => (others => 0));
	end record;


	function  GetWafeformShapes (abfH : in Header.ABFFileHeader) return Waveform_Shapes;
	procedure PutWafeformShapes (abfH : in out Header.ABFFileHeader; wf : in Waveform_Shapes);
	procedure PrintWaveformShapes (
			wf : in Waveform_Shapes;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True);

	--  just in case
	type WaveformRec is record
		numRuns : Natural := 0;
		numEpisodes : SweepIndex := 1;
			--  use RunsPerTrial and ActualEpisodes corresp
			--  EpisodesPerRun is not used, check that = ActualEpisodes
		--  NumberOfTrials       : Natural := 1;
		--  FirstEpisodeInRun    : Natural := 1;
			  --  just check that they are set

		NumSamplesPerEpisode : SampleCount := 0;
		EpisodeStartToStart  : TimeBase_sec := 0.0;
		RunStartToStart      : TimeBase_sec := 0.0;
		--  TrialStartToStart    : TimeBase_sec := 0.0;
			--  not used, there is only 1 in file!

		digiTrain : Digital_Train;
		wfShapes  : Waveform_Shapes;
	end record;

	function  GetWafeform (abfH : in Header.ABFFileHeader) return WaveformRec;
	procedure PutWaveform (abfH : in out Header.ABFFileHeader; wf : in WaveformRec);

	procedure PrintWaveform (
			wf : in WaveformRec;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True);


end ABF.Header.Waveform;