--
--
--  Ada Spec: abfheader_struct
--
--  Description: an Axon ABF header structure from AxonFSP32_1.8
--              converted to Ada
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2005
--
--  Copyright: See COPYING.LIB file that comes with this distribution
--
--
with C; use C;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO;

package ABF.Header is

	--  Some exceptions
	Inconsistent_Header_Params,
	Unsupported_Header_Params : exception;


	--
	--  Definition of the ABF header structure.
	--
	--  Unlike in C variant we define default component values here
	--
	type ABFFileHeader is record
		--  The total header length = 5120 bytes.
		--  GROUP #1 - File ID and size information. (40 bytes)
		lFileSignature       : C.long     := ABF_NATIVESIGNATURE;
		fFileVersionNumber   : C.C_float  := ABF_CURRENTVERSION;
		nOperationMode       : C.short    := ABF_GAPFREEFILE;
		lActualAcqLength     : C.long     := 0;
		nNumPointsIgnored    : C.short    := 0;
		lActualEpisodes      : C.long     := 0;
		lFileStartDate       : C.long     := 20051114;  -- YYYYMMDD
		lFileStartTime       : C.long     := 0000;
		lStopwatchTime       : C.long     := 0;
		fHeaderVersionNumber : C.C_float  := ABF_CURRENTVERSION;
		nFileType            : C.short    := ABF_ABFFILE;
		nMSBinFormat         : C.short    := 0;

		--  GROUP #2 - File Structure (78 bytes)
		lDataSectionPtr       : C.long  := ABFFileHeader'Size / ABF_BLOCKSIZE;
		lTagSectionPtr        : C.long  := 0;
		lNumTagEntries        : C.long  := 0;
		lScopeConfigPtr       : C.long  := 0;
		lNumScopes            : C.long  := 0;
		d_lDACFilePtr         : C.long  := 0;
		d_lDACFileNumEpisodes : C.long  := 0;
		sUnused001   : C.char_array (0 .. 3)  := (others => ' ');
		lDeltaArrayPtr        : C.long  := 0;
		lNumDeltas            : C.long  := 0;
		lVoiceTagPtr          : C.long  := 0;
		lVoiceTagEntries      : C.long  := 0;
		lUnused002            : C.long  := 0;
		lSynchArrayPtr        : C.long  := 0;
		lSynchArraySize       : C.long  := 0;
		nDataFormat           : C.short := 0;
		nSimultaneousScan     : C.short := 0;
		lStatisticsConfigPtr  : C.long  := 0;
		lAnnotationSectionPtr : C.long  := 0;
		lNumAnnotations       : C.long  := 0;
		sUnused003   : C.char_array (0 .. 1)  := (others => ' ');


		--  GROUP #3 - Trial hierarchy information (82 bytes)
		channel_count_acquired   : C.short    := 1;
			--  The number of input channels we acquired.
			--  Do not access directly - use CABFHeader::get_channel_count_acquired
			--  in earlier versions not used, nADCNumChannels has both..

		nADCNumChannels          : C.short    := 1;
			--  The number of input channels we recorded.
			--  Do not access directly - use CABFHeader::get_channel_count_recorded

		fADCSampleInterval       : C.C_float  := 100.0;
			--  The documentation says these two sample intervals are the interval between multiplexed samples,
			--  but not all digitisers work like that.
			--  Instead, these are the per-channel sample rate divided by the number of channels.
			--  If the user chose 100uS and has two channels, this value will be 50uS.

		fADCSecondSampleInterval : C.C_float  := 0.0;
			--  The two sample intervals must be an integer multiple (or submultiple) of each other.
			--  if (fADCSampleInterval > fADCSecondSampleInterval)
			--  	ASSERT(fmod(fADCSampleInterval, fADCSecondSampleInterval) == 0.0);
			--  if (fADCSecondSampleInterval, fADCSampleInterval)
			--  	ASSERT(fmod(fADCSecondSampleInterval, fADCSampleInterval) == 0.0);

		fSynchTimeUnit           : C.C_float  := 0.0;
		fSecondsPerRun           : C.C_float  := 0.0;

		lNumSamplesPerEpisode    : C.long     := 512;
			--  The total number of samples per episode, for the recorded channels only.
			--  This does not include channels which are acquired but not recorded.
			--  This is the number of samples per episode per channel, times the number of recorded channels.
			--  If you want the samples per episode for one channel, you must divide this by get_channel_count_recorded().

		lPreTriggerSamples       : C.long     := 16;
		lEpisodesPerRun          : C.long     := 1;
		lRunsPerTrial            : C.long     := 0;
		lNumberOfTrials          : C.long     := 0;
		nAveragingMode           : C.short    := 0;
		nUndoRunCount            : C.short    := 0;
		nFirstEpisodeInRun       : C.short    := 0;
		fTriggerThreshold        : C.C_float  := 0.0;
		nTriggerSource           : C.short    := 0;
		nTriggerAction           : C.short    := 0;
		nTriggerPolarity         : C.short    := 0;
		fScopeOutputInterval     : C.C_float  := 0.0;
		fEpisodeStartToStart     : C.C_float  := 0.0;
		fRunStartToStart         : C.C_float  := 0.0;
		fTrialStartToStart       : C.C_float  := 0.0;
		lAverageCount            : C.long     := 0;
		lClockChange             : C.long     := 0;
		nAutoTriggerStrategy     : C.short    := 0;

		--  GROUP #4 - Display Parameters (44 bytes)
		nDrawingStrategy      : C.short  := 0;
		nTiledDisplay         : C.short  := 0;
		nEraseStrategy        : C.short  := 1;
			--  N.B. Discontinued. Use scope config entry instead.
		nDataDisplayMode      : C.short  := ABF_DRAW_LINES;
		lDisplayAverageUpdate : C.long   := 0;
		nChannelStatsStrategy : C.short  := 0;
		lCalculationPeriod    : C.long   := 2; -- long(NewFH.fStatisticsPeriod / NewFH.fADCSampleInterval * 1E3F)
			--  N.B. Discontinued. Use fStatisticsPeriod.
		lSamplesPerTrace        : C.long     := 16384;
		lStartDisplayNum        : C.long     := 0;
		lFinishDisplayNum       : C.long     := 0;
		nMultiColor             : C.short    := 1; -- Boolean := True
		nShowPNRawData          : C.short    := 0;
		fStatisticsPeriod       : C.C_float  := 1.0;
		lStatisticsMeasurements : C.long     := 16#00000005#;
		nStatisticsSaveStrategy : C.short    := 0;

		--  GROUP #5 - Hardware information (16 bytes)
		fADCRange      : C.C_float  := 10.24;
		fDACRange      : C.C_float  := 10.24;
		lADCResolution : C.long     := 32768;
		lDACResolution : C.long     := 32768;

		--  GROUP #6 Environmental Information (118 bytes)
		nExperimentType          : C.short    := ABF_SIMPLEACQUISITION;
		d_nAutosampleEnable      : C.short    := 0;
		d_nAutosampleADCNum      : C.short    := 0;
		d_nAutosampleInstrument  : C.short    := 0;
		d_fAutosampleAdditGain   : C.C_float  := 0.0;
		d_fAutosampleFilter      : C.C_float  := 0.0;
		d_fAutosampleMembraneCap : C.C_float  := 0.0;
		nManualInfoStrategy      : C.short    := 0;
		fCellID1                 : C.C_float  := 0.0;
		fCellID2                 : C.C_float  := 0.0;
		fCellID3                 : C.C_float  := 0.0;
		sCreatorInfo   : C.char_array (0 .. ABF_CREATORINFOLEN - 1)     := (others => ' ');
		d_sFileComment : C.char_array (0 .. ABF_OLDFILECOMMENTLEN - 1)  := (others => ' ');
		nFileStartMillisecs : C.short  := 0;
			--  Milliseconds portion of lFileStartTime
		nCommentsEnable     : C.short  := 0;
		sUnused003a  : C.char_array (0 .. 7)  := (others => ' ');

		--  GROUP #7 - Multi-channel information (1044 = (160 + 384 + 488 + 12) bytes)
		nADCPtoLChannelMap : C.shortArray (ADC_PhysicalChannelIndex)  :=
				(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
		nADCSamplingSeq : SamplingSeqArray  := (others => ABF_UNUSED_CHANNEL);
				--  this is the only one addressed by high-level channel index
				--  the rest should cast through PhysicalChannelNum, to get the "real" index
				--  as all channels in ABFFileHeader are referenced in physical numbers
		sADCChannelName : C.char2DArray (ADC_PhysicalChannelIndex, 0 .. ABF_ADCNAMELEN - 1)  :=
			("AI #0     ", "AI #1     ", "AI #2     ", "AI #3     ",
			 "AI #4     ", "AI #5     ", "AI #6     ", "AI #7     ",
			 "AI #8     ", "AI #9     ", "AI #10    ", "AI #11    ",
			 "AI #12    ", "AI #13    ", "AI #14    ", "AI #15    ");
		sADCUnits       : C.char2DArray (ADC_PhysicalChannelIndex, 0 .. ABF_ADCUNITLEN - 1)
			:= (others => ('p', 'A', others => ' '));
		fADCProgrammableGain     : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 1.0);
		fADCDisplayAmplification : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 1.0);
		fADCDisplayOffset        : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		fInstrumentScaleFactor   : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 1.0);
		fInstrumentOffset        : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		fSignalGain              : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 1.0);
		fSignalOffset            : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		fSignalLowpassFilter     : C.floatArray (ADC_PhysicalChannelIndex)  := (others => ABF_FILTERDISABLED);
		fSignalHighpassFilter    : C.floatArray (ADC_PhysicalChannelIndex)  := (others => ABF_FILTERDISABLED);
		sDACChannelName  : C.char2DArray (0 .. ABF_DACCOUNT - 1, 0 .. ABF_DACNAMELEN - 1)
				:= ("AO #0     ", "AO #1     ", "AO #2     ", "AO #3     ");
		sDACChannelUnits : C.char2DArray (0 .. ABF_DACCOUNT - 1, 0 .. ABF_DACUNITLEN - 1)  := (others => "mV      ");
		fDACScaleFactor  : C.floatArray (0 .. ABF_DACCOUNT - 1)  := (others => 20.0);
		fDACHoldingLevel : C.floatArray (0 .. ABF_DACCOUNT - 1)  := (others =>  0.0);
		nSignalType : C.short           := 0;
		sUnused004  : C.char_array (0 .. 9)   := (others => ' ');

		--  GROUP #8 - Synchronous timer outputs (14 bytes)
		nOUTEnable        : C.short  := 0;
		nSampleNumberOUT1 : C.short  := 0;
		nSampleNumberOUT2 : C.short  := 0;
		nFirstEpisodeOUT  : C.short  := 0;
		nLastEpisodeOUT   : C.short  := 0;
		nPulseSamplesOUT1 : C.short  := 0;
		nPulseSamplesOUT2 : C.short  := 0;

		--  GROUP #9 - Epoch Waveform and Pulses (184 bytes)
		nDigitalEnable       : C.short  := 0;
		A_nWaveformSource    : C.short  := 0;
		nActiveDACChannel    : C.short  := 0;
		A_nInterEpisodeLevel : C.short  := 0;
		A_nEpochType         : C.shortArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0);
		A_fEpochInitLevel    : C.floatArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0.0);
		A_fEpochLevelInc     : C.floatArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0.0);
		A_nEpochInitDuration : C.shortArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0);
		A_nEpochDurationInc  : C.shortArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0);
		nDigitalHolding      : C.short  := 0;
		nDigitalInterEpisode : C.short  := 0;
		nDigitalValue : C.shortArray (0 .. ABF_EPOCHCOUNT - 1)  := (others => 0);
		sUnavailable1608   : C.char_array (0 .. 3)  := (others => ' ');
			--  was float fWaveformOffset;
		nDigitalDACChannel : C.short          := 0;
		sUnused005         : C.char_array (0 .. 5)  := (others => ' ');

		--  GROUP #10 - DAC Output File (98 bytes)
		d_fDACFileScale      : C.C_float := 1.0;
		d_fDACFileOffset     : C.C_float := 0.0;
		sUnused006    : C.char_array (0 .. 1)  := (others => ' ');
		d_nDACFileEpisodeNum : C.short   := 0;
		d_nDACFileADCNum     : C.short   := 0;
		d_sDACFilePath : C.char_array (0 .. ABF_DACFILEPATHLEN - 1) := (others => ' ');

		--  GROUP #11 - Conditioning pulse train (44 bytes)
		d_nConditEnable     : C.short    := 0;
		d_nConditChannel    : C.short    := 0;
		d_lConditNumPulses  : C.long     := 0;
		d_fBaselineDuration : C.C_float  := 0.0;
		d_fBaselineLevel    : C.C_float  := 0.0;
		d_fStepDuration     : C.C_float  := 0.0;
		d_fStepLevel        : C.C_float  := 0.0;
		d_fPostTrainPeriod  : C.C_float  := 0.0;
		d_fPostTrainLevel   : C.C_float  := 0.0;
		sUnused007   : C.char_array (0 .. 11)  := (others => ' ');

		--  GROUP #12 - Variable parameter user list ( 82 bytes)
		d_nParamToVary : C.short  := 0;
		d_sParamValueList : C.char_array (0 .. ABF_VARPARAMLISTLEN - 1)  := (others => ' ');

		--  GROUP #13 - Autopeak measurement (36 bytes)
		nAutopeakEnable     : C.short  := 0;
		nAutopeakPolarity   : C.short  := 0;
		nAutopeakADCNum     : C.short  := 0;
		nAutopeakSearchMode : C.short  := 0;
		lAutopeakStart      : C.long   := 0;
		lAutopeakEnd        : C.long   := 0;
		nAutopeakSmoothing  : C.short  := 0;
		nAutopeakBaseline   : C.short  := 0;
		nAutopeakAverage    : C.short  := 0;
		sUnavailable1866 : C.char_array (0 .. 1)  := (others => ' ');
			--  Was nAutopeakSaveStrategy, use nStatisticsSaveStrategy
		lAutopeakBaselineStart : C.long  := 0;
		lAutopeakBaselineEnd   : C.long  := 0;
		lAutopeakMeasurements  : C.long  := 0;

		--  GROUP #14 - Channel Arithmetic (52 bytes)
		nArithmeticEnable     : C.short    := 0;
		fArithmeticUpperLimit : C.C_float  := 0.0;
		fArithmeticLowerLimit : C.C_float  := 0.0;
		nArithmeticADCNumA    : C.short    := 0;
		nArithmeticADCNumB    : C.short    := 0;
		fArithmeticK1         : C.C_float  := 0.0;
		fArithmeticK2         : C.C_float  := 0.0;
		fArithmeticK3         : C.C_float  := 0.0;
		fArithmeticK4         : C.C_float  := 0.0;
		sArithmeticOperator : C.char_array (0 .. ABF_ARITHMETICOPLEN - 1)     := " +";
		sArithmeticUnits    : C.char_array (0 .. ABF_ARITHMETICUNITSLEN - 1)  := (others => ' ');
		fArithmeticK5         : C.C_float  := 0.0;
		fArithmeticK6         : C.C_float  := 0.0;
		nArithmeticExpression : C.short    := 0;
		sUnused008      : C.char_array (0 .. 1)  := (others => ' ');

		--  GROUP #15 - On-line subtraction (34 bytes)
		d_nPNEnable       : C.short    := 0;
		nPNPosition       : C.short    := 0;
		d_nPNPolarity     : C.short    := 0;
		nPNNumPulses      : C.short    := 0;
		d_nPNADCNum       : C.short    := 0;
		d_fPNHoldingLevel : C.C_float  := 0.0;
		fPNSettlingTime   : C.C_float  := 0.0;
		fPNInterpulse     : C.C_float  := 0.0;
		sUnused009  : C.char_array (0 .. 11) := (others => ' ');

		--  GROUP #16 - Unused space at end of header block (82 bytes)
		d_nListEnable    : C.short  := 0;

		nBellEnable      : C.shortArray (0 .. ABF_BELLCOUNT - 1)  := (others => 0);
		nBellLocation    : C.shortArray (0 .. ABF_BELLCOUNT - 1)  := (others => 0);
		nBellRepetitions : C.shortArray (0 .. ABF_BELLCOUNT - 1)  := (others => 0);

		nLevelHysteresis   : C.short  := DEFAULT_LEVEL_HYSTERESIS;
		lTimeHysteresis    : C.long   := DEFAULT_TIME_HYSTERESIS;
		nAllowExternalTags : C.short  := 0;

		nLowpassFilterType  : C.char_array (ADC_PhysicalChannelIndex)  := (others => ' ');
		nHighpassFilterType : C.char_array (ADC_PhysicalChannelIndex)  := (others => ' ');
		nAverageAlgorithm          : C.short    := 0;
		fAverageWeighting          : C.C_float  := 0.1;
		nUndoPromptStrategy        : C.short    := 0;
		nTrialTriggerSource        : C.short    := ABF_TRIALTRIGGER_NONE;
		nStatisticsDisplayStrategy : C.short    := 0;
		nExternalTagType           : C.short    := ABF_EXTERNALTAG;
		lHeaderSize                : C.long     := ABF_HEADERSIZE;
		dFileDuration              : C.double   := 0.0;
		nStatisticsClearStrategy   : C.short    := 0;
		--  Size of v1.5 header = 2048

		--  Extra parameters in v1.6

		--  GROUP #2 - File Structure (8 * 2 = 16)
		lDACFilePtr         : C.longArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		lDACFileNumEpisodes : C.longArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);

		--  EXTENDED GROUP #3 - Trial Hierarchy
		fFirstRunDelay : C.C_float := 0.0;
		sUnused010     : C.char_array (0 .. 5)  := (others => ' ');

		--  GROUP #7 - Multi-channel information (62 bytes)
		fDACCalibrationFactor : C.floatArray (0 .. ABF_DACCOUNT - 1)  := (others => 1.0);
		fDACCalibrationOffset : C.floatArray (0 .. ABF_DACCOUNT - 1)  := (others => 0.0);
		sUnused011            : C.char_array (0 .. 29)                := (others => ' ');

		--  // GROUP #17 - Trains parameters (160 bytes)
		lEpochPulsePeriod : C.long2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0));
		lEpochPulseWidth  : C.long2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0));

		--  GROUP #9 - Epoch Waveform and Pulses ( 412 bytes)
		nWaveformEnable    : C.shortArray   (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nWaveformSource    : C.shortArray   (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nInterEpisodeLevel : C.shortArray   (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nEpochType         : C.short2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0));
		fEpochInitLevel    : C.float2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0.0));
		fEpochLevelInc     : C.float2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0.0));
		lEpochInitDuration : C.long2DArray  (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0));
		lEpochDurationInc  : C.long2DArray  (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_EPOCHCOUNT - 1) := (others => (others => 0));
		nDigitalTrainValue : C.shortArray   (0 .. ABF_EPOCHCOUNT - 1)   := (others => 0);  -- 2 * 10 = 20 bytes
		nDigitalTrainActiveLogic : C.short := 0;  -- 2 bytes
		sUnused012         : C.char_array (0 .. 17)  := (others => ' ');

		--  GROUP #10 - DAC Output File (552 bytes)
		fDACFileScale      : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 1.0);
		fDACFileOffset     : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		lDACFileEpisodeNum : C.longArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nDACFileADCNum     : C.shortArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		sDACFilePath : C.char2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_PATHLEN - 1)
				:= (others => (others => ' '));
		sUnused013   : C.char_array (0 .. 11)  := (others => ' ');

		--  GROUP #11 - Presweep (conditioning) pulse train (100 bytes)
		nConditEnable     : C.shortArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		lConditNumPulses  : C.longArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		fBaselineDuration : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		fBaselineLevel    : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		fStepDuration     : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		fStepLevel        : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		fPostTrainPeriod  : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		fPostTrainLevel   : C.floatArray (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		sUnused014        : C.char_array (0 .. 39)                    := (others => ' ');

		--  GROUP #12 - Variable parameter user list (1096 bytes)
		nULEnable      : C.shortArray (0 .. ABF_USERLISTCOUNT - 1)  := (others => 0);
		nULParamToVary : C.shortArray (0 .. ABF_USERLISTCOUNT - 1)  := (others => 0);
		sULParamValueList : C.char2DArray (0 .. ABF_USERLISTCOUNT - 1, 0 .. ABF_USERLISTLEN - 1)
				:= (others => (others => ' '));
		nULRepeat      : C.shortArray (0 .. ABF_USERLISTCOUNT - 1)  := (others => 0);
		sUnused015     : C.char_array (0 .. 47)                    := (others => ' ');

		--  GROUP #15 - On-line subtraction (56 bytes)
		nPNEnable         : C.shortArray  (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nPNPolarity       : C.shortArray  (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		dd_nPNADCNum      : C.shortArray  (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		fPNHoldingLevel   : C.floatArray  (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0.0);
		nPNNumADCChannels : C.shortArray  (0 .. ABF_WAVEFORMCOUNT - 1)  := (others => 0);
		nPNADCSamplingSeq : C.char2DArray (0 .. ABF_WAVEFORMCOUNT - 1, ADC_PhysicalChannelIndex)
				:= (others => (others => ' '));

		--  GROUP #6 Environmental Information  ( 898 bytes)
		nTelegraphEnable         : C.shortArray (ADC_PhysicalChannelIndex)  := (others => 0);
		nTelegraphInstrument     : C.shortArray (ADC_PhysicalChannelIndex)  := (others => 0);
		fTelegraphAdditGain      : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		fTelegraphFilter         : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		fTelegraphMembraneCap    : C.floatArray (ADC_PhysicalChannelIndex)  := (others => 0.0);
		nTelegraphMode           : C.shortArray (ADC_PhysicalChannelIndex)  := (others => 0);
		nTelegraphDACScaleFactorEnable : C.shortArray (0 .. ABF_DACCOUNT - 1) := (others => 0);
		sUnused016a              : C.char_array (0 .. 23) := (others => ' ');

		nAutoAnalyseEnable     : C.short  := 0;
		sAutoAnalysisMacroName : C.char_array (0 .. ABF_MACRONAMELEN - 1)  := (others => ' ');
		sProtocolPath          : C.char_array (0 .. ABF_PATHLEN - 1)       := (others => ' ');

		sFileComment : C.char_array (0 .. ABF_FILECOMMENTLEN - 1)  := (others => ' ');
		fInstrumentHoldingLevel : C.floatArray (0 .. ABF_DACCOUNT - 1)     := (others => 0.0);
		FileGUID                : C.char_array (0 .. 15)                   := (others => ' ');
		ulFileCRC               : C.unsigned_long          := 0;
		sModifierInfo           : C.char_array (0 .. ABF_CREATORINFOLEN - 1) := (others => ' ');
		sUnused6                : C.char_array (0 .. 75)   := (others => ' ');

		--  EXTENDED GROUP #13 - Statistics measurements (388 bytes)
		nStatsEnable            : C.short  := 0;
		nStatsActiveChannels    : C.unsigned_short := 0; -- Active stats channel bit flag
		nStatsSearchRegionFlags : C.unsigned_short := 0; -- Active stats region bit flag
		nStatsSelectedRegion    : C.short  := 0;
		d_nStatsSearchMode      : C.short  := 0;
		nStatsSmoothing         : C.short  := 0;
		nStatsSmoothingEnable   : C.short  := 0;
		nStatsBaseline          : C.short  := 0;
		lStatsBaselineStart     : C.long   := 0;
		lStatsBaselineEnd       : C.long   := 0;
		lStatsMeasurements     : C.longArray  (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
				--  Measurement bit flag for each region
		lStatsStart            : C.longArray  (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		lStatsEnd              : C.longArray  (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		nRiseBottomPercentile  : C.shortArray (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		nRiseTopPercentile     : C.shortArray (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		nDecayBottomPercentile : C.shortArray (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		nDecayTopPercentile    : C.shortArray (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
		nStatsChannelPolarity  : C.shortArray (ADC_PhysicalChannelIndex)      := (others => 0);
		nStatsSearchMode       : C.shortArray (0 .. ABF_STATS_REGIONS - 1) := (others => 0);
				--  Stats mode per region: mode is cursor region, epoch etc
		sUnused018 : C.char_array (0 .. 155) := (others => ' ');

		--  GROUP #18 - Application version data (16 bytes)
		nCreatorMajorVersion   : C.short  := 0;
		nCreatorMinorVersion   : C.short  := 0;
		nCreatorBugfixVersion  : C.short  := 0;
		nCreatorBuildVersion   : C.short  := 0;
		nModifierMajorVersion  : C.short  := 0;
		nModifierMinorVersion  : C.short  := 0;
		nModifierBugfixVersion : C.short  := 0;
		nModifierBuildVersion  : C.short  := 0;

		--  // GROUP #19 - LTP protocol (14 bytes)
		nLTPType              : C.short   := 0;
		nLTPUsageOfDAC        : C.shortArray (0 .. ABF_WAVEFORMCOUNT - 1) := (others => 0);
		nLTPPresynapticPulses : C.shortArray (0 .. ABF_WAVEFORMCOUNT - 1) := (others => 0);
		sUnused020            : C.char_array (0 .. 3)                     := (others => ' ');

		--  // GROUP #20 - Digidata 132x Trigger out flag. (8 bytes)
		nDD132xTriggerOut : C.short := 0;
		sUnused021        : C.char_array (0 .. 5) := (others => ' ');

		--  // GROUP #21 - Epoch resistance (40 bytes)
		sEpochResistanceSignalName : C.char2DArray (0 .. ABF_WAVEFORMCOUNT - 1, 0 .. ABF_ADCNAMELEN - 1)
			:= (others => (others => ' '));
		nEpochResistanceState      : C.shortArray  (0 .. ABF_WAVEFORMCOUNT - 1) := (others => 0);
		sUnused022                 : C.char_array  (0 .. 15)                    := (others => ' ');

		--  // GROUP #22 - Alternating episodic mode (58 bytes)
		nAlternateDACOutputState     : C.short  := 0;
		nAlternateDigitalValue       : C.shortArray (0 .. ABF_EPOCHCOUNT - 1) := (others => 0);
		nAlternateDigitalTrainValue  : C.shortArray (0 .. ABF_EPOCHCOUNT - 1) := (others => 0);
		nAlternateDigitalOutputState : C.short  := 0;
		sUnused023 : C.char_array (0 .. 14 - 1) := (others => ' ');

		--  // GROUP #23 - Post-processing actions (210 bytes, including sUnused2048)
		fPostProcessLowpassFilter     : C.floatArray (ADC_PhysicalChannelIndex);
		nPostProcessLowpassFilterType : C.char_array (ADC_PhysicalChannelIndex);

		--  // 6014 header bytes allocated + 130 header bytes not allocated
		sUnused2048 : C.char_array (0 .. 129) := (others => ' ');
	end record;  -- ABFFileHeader, Size = 6144
	pragma Pack (ABFFileHeader);


	--------------------------------------------
	--  Some initialization and IO routines
	--------------------------------------------

	function  Empty_GapFree_ABFFileHeader return ABFFileHeader;
		--  fills the record with basic params as per Axon's Initialize

	procedure PrintPrincialParams (abfH : in ABFFileHeader;
			File : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
		--  outputs some basic fields to stdout

	procedure Write_Header (abfH : in ABFFileHeader; FileName : String);
	function  Read_Header  (FileName : String) return ABFFileHeader;
		--  basic IO, mostly for testing purposes, files are closed upon leaving the procedure..
		--   Write_Header opens files if exists and writes header over, otherwise new file is created

	--  type ABF_FileOpen_Mode is (ReadOnly, WriteOnly, ReadWrite);
	--
	--  procedure Open_AbfFile (FileName: in String; File : out Stream_Io.File_Type;
	-- 		Mode : in ABF_FileOpen_Mode; );


	------ some type checks and conversions  ---------------
	function PhysicalChannelNum(abfH : Header.ABFFileHeader; nChan : ADC_ChannelIndex)
		return ADC_PhysicalChannelIndex;

	------  handling of Mode_Descriptors from parent -------
	function  GetModeDescriptors (abfH : Header.ABFFileHeader) return Mode_Descriptors;
	procedure PrintModeDescriptors (md : in Mode_Descriptors; File : in Ada.Text_IO.File_Type);

	---------------------------------------------------
	--  data handling, scaling factors, waveforms, etc.
	---------------------------------------------------

	function  GetADCtoUUFactors (abfH : in ABFFileHeader; nChannel : in ADC_ChannelIndex)
			return ADCtoUUFactors;
	--  Calculates the scaling factors used to convert DAC values to UserUnits.
	--   nChannel        - The physical channel number to get the factors for.
	--   returns scale and offset. UserUnits = ADCValue * fADCToUUFactor + fADCToUUShift;

	procedure ClipADCUUValue (abfH : in ABFFileHeader; nChannel : in ADC_ChannelIndex;
			value : in out ADC_UserUnits);
	--  Limits the UU value to the range of the A/D converter.

	--  same for the following two routines, with DAC parameters
-- 	function GetDADCtoUUFactors (abfH : in ABFFileHeader; nChannel : ADC_ChannelIndex)
-- 			return DACtoUUFactors;
--
-- 	procedure ClipDACUUValue (abfH : in ABFFileHeader; nChannel : in ADC_ChannelIndex;
-- 			value : in out Float);




	------------------------------------------------------------------------
	---------   Some additional structures -----------
	------------------------------------------------------------------------

	--  ABFSignal struct (whatever it is)
	type ABFSignal is record
		szName      : C.char_array (0 .. ABF_ADCNAMELEN - 1 + 2);
			--  ABF name length + '\0' + 1 for alignment.
		nMxOffset   : C.short;  -- Offset of the signal in the sampling sequence.
		nPenWidth   : C.char;  -- Pen width in pixels.
		bDrawPoints : C.char;  -- TRUE = Draw disconnected points
		bHidden     : C.char;  -- TRUE = Hide the trace.
		bFloatData  : C.char;  -- TRUE = Floating point pseudo channel
		fVertProportion : C.C_float;  -- Relative proportion of client area to use
		fDisplayGain    : C.C_float;  -- Display gain of trace in UserUnits
		fDisplayOffset  : C.C_float;  -- Display offset of trace in UserUnits

	--   float    fUUTop;                          // Top of window in UserUnits
	--   float    fUUBottom;                       // Bottom of window in UserUnits
	end record;  -- Size = 34

	--
	--  Definition of the ABF synch array structure
	--
	type ABFSynch is record
		lStart  : C.long;  -- Start of the episode/event in fSynchTimeUnit units.
		lLength : C.long;  -- Length of the episode/event in multiplexed samples.
	end record;  -- Size = 8
	pragma Pack (ABFSynch);


	--
	--  Definition of the ABF Tag structure
	--
	type ABFTag is record
		lTagTime : C.long;
			--  Time at which the tag was entered in fSynchTimeUnit units.
		sComment : C.char_array (0 .. ABF_TAGCOMMENTLEN);
			--  Optional tag comment.
		nTagType : C.short;
			--  Type of tag ABF_TIMETAG, ABF_COMMENTTAG, ABF_EXTERNALTAG or ABF_VOICETAG.
		nVoiceTagNumber : C.short;
			--  If nTagType=ABF_VOICETAG, this is the number of this voice tag.
	end record;  -- Size = 64
	pragma Pack (ABFTag);

	--  Comment inserted for externally acquired tags (expanded with spaces to ABF_TAGCOMMENTLEN).
	ABF_EXTERNALTAGCOMMENT : constant C.char_array := "<External>";
	ABF_VOICETAGCOMMENT    : constant C.char_array := "<Voice Tag>";

	--
	--  Constants for nCompressionType in the ABFVoiceTagInfo structure.
	--
	ABF_COMPRESSION_NONE   : constant := 0;
	ABF_COMPRESSION_PKWARE : constant := 1;
	ABF_COMPRESSION_MPEG   : constant := 2;

	--
	--  Definition of the ABFVoiceTagInfo structure.
	--
	type ABFVoiceTagInfo is record
		lTagNumber  : C.long;  -- The tag number that corresponds to this VoiceTag
		lFileOffset : C.long;  -- Offset to this tag within the VoiceTag block
		lUncompressedSize : C.long;  -- Size of the voice tag expanded.
		lCompressedSize   : C.long;  -- Compressed size of the tag.
		nCompressionType  : C.short;  -- Compression method used.
		nSampleSize       : C.short;  -- Size of the samples acquired.
		lSamplesPerSecond : C.long;  -- Rate at which the sound was acquired.
	end record;  -- Size 32
	pragma Pack (ABFVoiceTagInfo);

	--
	--  The size of the buffers to be passed to ABFH_GetWaveformVertor
	--
	ABFH_MAXVECTORS : constant := 30;


	--
	--  Error return values that may be returned by the ABFH_xxx functions.
	--
	ABFH_FIRSTERRORNUMBER  : constant := 2001;
	ABFH_EHEADERREAD       : constant := 2001;
	ABFH_EHEADERWRITE      : constant := 2002;
	ABFH_EINVALIDFILE      : constant := 2003;
	ABFH_EUNKNOWNFILETYPE  : constant := 2004;
	ABFH_CHANNELNOTSAMPLED : constant := 2005;
	ABFH_EPOCHNOTPRESENT   : constant := 2006;
	ABFH_ENOWAVEFORM       : constant := 2007;
	ABFH_EDACFILEWAVEFORM  : constant := 2008;
	ABFH_ENOMEMORY         : constant := 2009;
	ABFH_BADSAMPLEINTERVAL : constant := 2010;
	ABFH_BADSECONDSAMPLEINTERVAL : constant := 2011;
	ABFH_BADSAMPLEINTERVALS : constant := 2012;
	ABFH_ENOCONDITTRAINS    : constant := 2013;
	ABFH_EMETADURATION      : constant := 2014;
	ABFH_ECONDITNUMPULSES   : constant := 2015;
	ABFH_ECONDITBASEDUR     : constant := 2016;
	ABFH_ECONDITBASELEVEL   : constant := 2017;
	ABFH_ECONDITPOSTTRAINDUR : constant := 2018;
	ABFH_ECONDITPOSTTRAINLEVEL : constant := 2019;
	ABFH_ESTART2START     : constant := 2020;
	ABFH_EINACTIVEHOLDING : constant := 2021;
	ABFH_EINVALIDCHARS    : constant := 2022;
	ABFH_ENODIG           : constant := 2023;
	ABFH_EDIGHOLDLEVEL    : constant := 2024;
	ABFH_ENOPNPULSES      : constant := 2025;
	ABFH_EPNNUMPULSES     : constant := 2026;
	ABFH_ENOEPOCH         : constant := 2027;
	ABFH_EEPOCHLEN        : constant := 2028;
	ABFH_EEPOCHINITLEVEL  : constant := 2029;
	ABFH_EDIGLEVEL        : constant := 2030;
	ABFH_ECONDITSTEPDUR   : constant := 2031;
	ABFH_ECONDITSTEPLEVEL : constant := 2032;
	ABFH_EINVALIDBINARYCHARS : constant := 2033;
	ABFH_EBADWAVEFORM     : constant := 2034;

end ABF.Header;