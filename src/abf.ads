--
--
--  Ada Spec: ab
--
--  Description: Top level for the ABF library.
--  Keeps most of the constants + some generic types
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2005
--
--  Copyright: See COPYING file that comes with this distribution
--
--

with Ada.Text_IO;

with C; use C;

package ABF is
--pragma Pure (ABF);

	--  Some exceptions and notifications
    Not_Implemented : exception;

    Debug : Boolean;


	-------------------------------------
	--  Constants from Axon
	-------------------------------------

	--  Misc constants from abfheadr.cpp
	--
	DEFAULT_LEVEL_HYSTERESIS : constant := 64;
	DEFAULT_TIME_HYSTERESIS  : constant := 1;


	--
	--  Constants for nTagType in the ABFTag structure.
	--
	ABF_TIMETAG       : constant := 0;
	ABF_COMMENTTAG    : constant := 1;
	ABF_EXTERNALTAG   : constant := 2;
	ABF_VOICETAG      : constant := 3;
	ABF_NEWFILETAG    : constant := 4;
	ABF_ANNOTATIONTAG : constant := 5;
		--  Same as a comment tag except that nAnnotationIndex holds
		--  the index of the annotation that holds extra information.

	--
	--  Constants used in defining the ABF file header
	--
	ABF_ADCCOUNT          : constant := 16;
	ABF_DACCOUNT          : constant := 4;
	ABF_WAVEFORMCOUNT     : constant := 2;
	ABF_EPOCHCOUNT        : constant := 10;
	ABF_BELLCOUNT         : constant := 2;
	ABF_ADCUNITLEN        : constant := 8;
	ABF_ADCNAMELEN        : constant := 10;
	ABF_DACUNITLEN        : constant := 8;
	ABF_DACNAMELEN        : constant := 10;
	ABF_VARPARAMLISTLEN   : constant := 80;
	ABF_USERLISTLEN       : constant := 256;
	ABF_USERLISTCOUNT     : constant := 4;
	ABF_OLDFILECOMMENTLEN : constant := 56;
	ABF_FILECOMMENTLEN    : constant := 128;

	ABF_CREATORINFOLEN     : constant := 16;
	ABF_OLDDACFILENAMELEN  : constant := 12;
	ABF_OLDDACFILEPATHLEN  : constant := 60;
	ABF_DACFILEPATHLEN     : constant := 84;
	ABF_PATHLEN            : constant := 256;
	ABF_ARITHMETICOPLEN    : constant := 2;
	ABF_ARITHMETICUNITSLEN : constant := 8;
	ABF_TAGCOMMENTLEN      : constant := 56;
	ABF_LONGDESCRIPTIONLEN : constant := 56;
	ABF_NOTENAMELEN        : constant := 10;
	ABF_NOTEVALUELEN       : constant := 8;
	ABF_NOTEUNITSLEN       : constant := 8;
	ABF_BLOCKSIZE          : constant := 512;
	ABF_MACRONAMELEN       : constant := 64;

	ABF_CURRENTVERSION   : constant := 1.65;
	ABF_PREVIOUSVERSION  : constant := 1.5;
	ABF_V16              : constant := 1.6;
	ABF_HEADERSIZE       : constant := 6144;
	ABF_OLDHEADERSIZE    : constant := 2048;
	ABF_NATIVESIGNATURE  : constant := 16#20464241#;
	ABF_REVERSESIGNATURE : constant := 16#41424620#;

	PCLAMP6_MAXSWEEPLENGTH      : constant := 16384;
	PCLAMP7_MAXSWEEPLEN_PERCHAN : constant := 103224;

	ABF_MAX_TRIAL_SAMPLES : constant := 16#7FFFFFFF#;
	--  INT_MAX is used instead of UINT_MAX because of the signed
	--  values in the ABF header.

	ABF_MAX_SWEEPS_PER_AVERAGE : constant := 65500;
	--  cumulative average (nAverageAlgorithm=ABF_INFINITEAVERAGE).

	ABF_STATS_REGIONS    : constant := 8; -- The number of independent statistics regions.
	ABF_BASELINE_REGIONS : constant := 1; -- The number of independent baseline regions.


	ABF_OLDPCLAMP : constant := ABF_NATIVESIGNATURE;

	--
	--  constant definitions for nFileType
	--
	ABF_ABFFILE : constant := 1;
	ABF_FETCHEX : constant := 2;
	ABF_CLAMPEX : constant := 3;

	--
	--  constant definitions for nDataFormat
	--
	ABF_INTEGERDATA : constant := 0;
	ABF_FLOATDATA   : constant := 1;

	--
	--  constant definitions for nOperationMode
	--
	ABF_VARLENEVENTS : constant := 1;
	ABF_FIXLENEVENTS : constant := 2;
	ABF_LOSSFREEOSC  : constant := 2;
	ABF_GAPFREEFILE  : constant := 3;
	ABF_HIGHSPEEDOSC : constant := 4;
	ABF_WAVEFORMFILE : constant := 5;

	--
	--  constant definitions for nParamToVary
	--
	ABF_CONDITNUMPULSES         : constant := 0;
	ABF_CONDITBASELINEDURATION  : constant := 1;
	ABF_CONDITBASELINELEVEL     : constant := 2;
	ABF_CONDITSTEPDURATION      : constant := 3;
	ABF_CONDITSTEPLEVEL         : constant := 4;
	ABF_CONDITPOSTTRAINDURATION : constant := 5;
	ABF_CONDITPOSTTRAINLEVEL    : constant := 6;
	ABF_EPISODESTARTTOSTART     : constant := 7;
	ABF_INACTIVEHOLDING         : constant := 8;
	ABF_DIGITALHOLDING          : constant := 9;
	ABF_PNNUMPULSES             : constant := 10;
	ABF_PARALLELVALUE           : constant := 11;
	ABF_EPOCHINITLEVEL          : constant := 21;
	ABF_EPOCHINITDURATION       : constant := 31;

	--
	--  Constants for nAveragingMode
	--
	ABF_NOAVERAGING     : constant := 0;
	ABF_SAVEAVERAGEONLY : constant := 1;
	ABF_AVERAGESAVEALL  : constant := 2;

	--
	--  Constants for nAverageAlgorithm
	--
	ABF_INFINITEAVERAGE : constant := 0;
	ABF_SLIDINGAVERAGE  : constant := 1;

	--
	--  Constants for nEpochType
	--
	ABF_EPOCHDISABLED : constant := 0;
	ABF_EPOCHSTEPPED  : constant := 1;
	ABF_EPOCHRAMPED   : constant := 2;

	--
	--  Constants for nWaveformSource
	--
	ABF_WAVEFORMDISABLED   : constant := 0;
	ABF_EPOCHTABLEWAVEFORM : constant := 1;
	ABF_DACFILEWAVEFORM    : constant := 2;

	--
	--  Constants for nInterEpisodeLevel & nDigitalInterEpisode
	--
	ABF_INTEREPI_USEHOLDING   : constant := 0;
	ABF_INTEREPI_USELASTEPOCH : constant := 1;

	--
	--  Constants for nExperimentType
	--
	ABF_VOLTAGECLAMP      : constant := 0;
	ABF_CURRENTCLAMP      : constant := 1;
	ABF_SIMPLEACQUISITION : constant := 2;

	--
	--  Constants for nAutosampleEnable
	--
	ABF_AUTOSAMPLEDISABLED  : constant := 0;
	ABF_AUTOSAMPLEAUTOMATIC : constant := 1;
	ABF_AUTOSAMPLEMANUAL    : constant := 2;

	--
	--  Constants for nAutosampleInstrument
	--
	ABF_INST_UNKNOWN       : constant := 0;
	ABF_INST_AXOPATCH1     : constant := 1;
	ABF_INST_AXOPATCH1_1   : constant := 2;
	ABF_INST_AXOPATCH1B    : constant := 3;
	ABF_INST_AXOPATCH1B_1  : constant := 4;
	ABF_INST_AXOPATCH201   : constant := 5;
	ABF_INST_AXOPATCH202   : constant := 6;
	ABF_INST_GENECLAMP     : constant := 7;
	ABF_INST_DAGAN3900     : constant := 8;
	ABF_INST_DAGAN3900A    : constant := 9;
	ABF_INST_DAGANCA1_1    : constant := 10;
	ABF_INST_DAGANCA1      : constant := 11;
	ABF_INST_DAGANCA10     : constant := 12;
	ABF_INST_WARNER_OC725  : constant := 13;
	ABF_INST_WARNER_OC725C : constant := 14;
	ABF_INST_AXOPATCH200B  : constant := 15;
	ABF_INST_DAGANPCONE0_1 : constant := 16;
	ABF_INST_DAGANPCONE1   : constant := 17;
	ABF_INST_DAGANPCONE10  : constant := 18;
	ABF_INST_DAGANPCONE100 : constant := 19;
	ABF_INST_WARNER_BC525C : constant := 20;
	ABF_INST_WARNER_PC505  : constant := 21;
	ABF_INST_WARNER_PC501  : constant := 22;
	ABF_INST_DAGANCA1_05   : constant := 23;
	ABF_INST_MULTICLAMP700 : constant := 24;
	ABF_INST_TURBO_TEC     : constant := 25;

	--
	--  Constants for nManualInfoStrategy
	--
	ABF_ENV_DONOTWRITE      : constant := 0;
	ABF_ENV_WRITEEACHTRIAL  : constant := 1;
	ABF_ENV_PROMPTEACHTRIAL : constant := 2;

	--
	--  Constants for nTriggerSource
	--
	ABF_TRIGGERLINEINPUT    : constant := -5;
	ABF_TRIGGERTAGINPUT     : constant := -4;
	ABF_TRIGGERFIRSTCHANNEL : constant := -3;
	ABF_TRIGGEREXTERNAL     : constant := -2;
	ABF_TRIGGERSPACEBAR     : constant := -1;
	--  >=0 = ADC channel to trigger off.

	--
	--  Constants for nTrialTriggerSource
	--
	ABF_TRIALTRIGGER_SWSTARTONLY : constant := -6;
	ABF_TRIALTRIGGER_SWSTARTSTOP : constant := -5;
	ABF_TRIALTRIGGER_LINEINPUT   : constant := -4;
	ABF_TRIALTRIGGER_SPACEBAR    : constant := -3;
	ABF_TRIALTRIGGER_EXTERNAL    : constant := -2;
	ABF_TRIALTRIGGER_NONE        : constant := -1;
	--  >=0 = ADC channel to trigger off.    // Not implemented as yet...

	--
	--  Constants for nTriggerPolarity.
	--
	ABF_TRIGGER_RISINGEDGE  : constant := 0;
	ABF_TRIGGER_FALLINGEDGE : constant := 1;

	--
	--  Constants for nTriggerAction
	--
	ABF_TRIGGER_STARTEPISODE : constant := 0;
	ABF_TRIGGER_STARTRUN     : constant := 1;
	ABF_TRIGGER_STARTTRIAL   : constant := 2;

	--
	--  Constants for nDrawingStrategy
	--
	ABF_DRAW_NONE       : constant := 0;
	ABF_DRAW_REALTIME   : constant := 1;
	ABF_DRAW_FULLSCREEN : constant := 2;
	ABF_DRAW_ENDOFRUN   : constant := 3;

	--
	--  Constants for nTiledDisplay
	--
	ABF_DISPLAY_SUPERIMPOSED : constant := 0;
	ABF_DISPLAY_TILED        : constant := 1;

	--
	--  Constants for nDataDisplayMode
	--
	ABF_DRAW_POINTS : constant := 0;
	ABF_DRAW_LINES  : constant := 1;

	--
	--  Constants for nArithmeticExpression
	--
	ABF_SIMPLE_EXPRESSION : constant := 0;
	ABF_RATIO_EXPRESSION  : constant := 1;

	--
	--  Constants for nLowpassFilterType & nHighpassFilterType
	--
	ABF_FILTER_NONE        : constant := 0;
	ABF_FILTER_EXTERNAL    : constant := 1;
	ABF_FILTER_SIMPLE_RC   : constant := 2;
	ABF_FILTER_BESSEL      : constant := 3;
	ABF_FILTER_BUTTERWORTH : constant := 4;

	--
	--  Constants for nPNPosition
	--
	ABF_PN_BEFORE_EPISODE : constant := 0;
	ABF_PN_AFTER_EPISODE  : constant := 1;

	--
	--  Constants for nPNPolarity
	--
	ABF_PN_OPPOSITE_POLARITY : constant := -1;
	ABF_PN_SAME_POLARITY     : constant := 1;

	--
	--  Constants for nAutopeakPolarity
	--
	ABF_PEAK_NEGATIVE : constant := -1;
	ABF_PEAK_ABSOLUTE : constant := 0;
	ABF_PEAK_POSITIVE : constant := 1;

	--
	--  Constants for nAutopeakSearchMode
	--
	ABF_PEAK_SEARCH_SPECIFIED : constant := -2;
	ABF_PEAK_SEARCH_ALL       : constant := -1;
	--  nAutopeakSearchMode 0..9   = epoch in waveform 0's epoch table
	--  nAutopeakSearchMode 10..19 = epoch in waveform 1's epoch table

	--
	--  Constants for nAutopeakBaseline
	--
	ABF_PEAK_BASELINE_SPECIFIED    : constant := -3;
	ABF_PEAK_BASELINE_NONE         : constant := -2;
	ABF_PEAK_BASELINE_FIRSTHOLDING : constant := -1;
	ABF_PEAK_BASELINE_LASTHOLDING  : constant := -4;

	--
	--  Constants for lAutopeakMeasurements
	--
	ABF_PEAK_MEASURE_PEAK           : constant := 16#00000001#;
	ABF_PEAK_MEASURE_PEAKTIME       : constant := 16#00000002#;
	ABF_PEAK_MEASURE_ANTIPEAK       : constant := 16#00000004#;
	ABF_PEAK_MEASURE_ANTIPEAKTIME   : constant := 16#00000008#;
	ABF_PEAK_MEASURE_MEAN           : constant := 16#00000010#;
	ABF_PEAK_MEASURE_STDDEV         : constant := 16#00000020#;
	ABF_PEAK_MEASURE_INTEGRAL       : constant := 16#00000040#;
	ABF_PEAK_MEASURE_LEFTSLOPE      : constant := 16#00000080#;
	ABF_PEAK_MEASURE_LEFTSLOPETIME  : constant := 16#00000100#;
	ABF_PEAK_MEASURE_RIGHTSLOPE     : constant := 16#00000200#;
	ABF_PEAK_MEASURE_RIGHTSLOPETIME : constant := 16#00000400#;
	ABF_PEAK_MEASURE_RISETIME       : constant := 16#00000800#;
	ABF_PEAK_MEASURE_DECAYTIME      : constant := 16#00001000#;
	ABF_PEAK_MEASURE_HALFWIDTH      : constant := 16#00002000#;
	ABF_PEAK_MEASURE_BASELINE       : constant := 16#00004000#;
	ABF_PEAK_MEASURE_ALL            : constant := 16#00007FFF#;

	--
	--  Constants for lStatisticsMeasurements
	--
	ABF_STATISTICS_ABOVETHRESHOLD : constant := 16#00000001#;
	ABF_STATISTICS_EVENTFREQUENCY : constant := 16#00000002#;
	ABF_STATISTICS_MEANOPENTIME   : constant := 16#00000004#;
	ABF_STATISTICS_MEANCLOSEDTIME : constant := 16#00000008#;
	ABF_STATISTICS_ALL            : constant := 16#0000000F#;

	--
	--  Constants for nStatisticsSaveStrategy
	--
	ABF_STATISTICS_NOAUTOSAVE  : constant := 0;
	ABF_STATISTICS_AUTOSAVE    : constant := 1;
	ABF_STATISTICS_AUTOSAVE_AUTOCLEAR : constant := 2;

	--
	--  Constants for nStatisticsDisplayStrategy
	--
	ABF_STATISTICS_DISPLAY   : constant := 0;
	ABF_STATISTICS_NODISPLAY : constant := 1;

	--
	--  Constants for nStatisticsClearStrategy
	--  determines whether to clear statistics after saving.
	--
	ABF_STATISTICS_NOCLEAR : constant := 0;
	ABF_STATISTICS_CLEAR   : constant := 1;

	--
	--  Constants for nDACFileEpisodeNum
	--
	ABF_DACFILE_SKIPFIRSTSWEEP : constant := -1;
	ABF_DACFILE_USEALLSWEEPS   : constant := 0;
	--  >0 = The specific sweep number.

	--
	--  Constants for nUndoPromptStrategy
	--
	ABF_UNDOPROMPT_ONABORT : constant := 0;
	ABF_UNDOPROMPT_ALWAYS  : constant := 1;

	--
	--  Constants for nAutoAnalyseEnable
	--
	ABF_AUTOANALYSE_DISABLED : constant := 0;
	ABF_AUTOANALYSE_DEFAULT  : constant := 1;
	ABF_AUTOANALYSE_RUNMACRO : constant := 2;

	--
	--  Miscellaneous constants
	--
	ABF_FILTERDISABLED : constant := 100000.0;
	ABF_UNUSED_CHANNEL : constant := -1;

	--
	--  The output sampling sequence identifier for a seperate digital out channel.
	--
	ABF_DIGITAL_OUT_CHANNEL : constant := -1;
	ABF_PADDING_OUT_CHANNEL : constant := -2;

	--  maximum values for various parameters (used by ABFH_CheckUserList).
	ABF_CTPULSECOUNT_MAX          : constant := 10000;
	ABF_CTBASELINEDURATION_MAX    : constant := 100000.0;
	ABF_CTSTEPDURATION_MAX        : constant := 100000.0;
	ABF_CTPOSTTRAINDURATION_MAX   : constant := 100000.0;
	ABF_SWEEPSTARTTOSTARTTIME_MAX : constant := 100000.0;
	ABF_PNPULSECOUNT_MAX          : constant := 8;
	ABF_DIGITALVALUE_MAX          : constant := 16#FF#;
	ABF_EPOCHDIGITALVALUE_MAX     : constant := 16#0F#;


	------------------------------------------------------------
	--  Some higher order types
	------------------------------------------------------------

	type ADC_UserUnits is new C.C_float;
	type ADCtoUUFactor is new C.C_float;

	function "*" (Left : C.short; Right : ADCtoUUFactor) return ADC_UserUnits;
	function "*" (Left : ADCtoUUFactor; Right : C.short) return ADC_UserUnits;
	function "*" (Left : C.long; Right : ADCtoUUFactor)  return ADC_UserUnits;
	function "*" (Left : ADCtoUUFactor; Right : C.long)  return ADC_UserUnits;


	package UserUnits_IO is new Ada.Text_IO.Float_IO(ADC_UserUnits);

	--  rescaling is done as follows:
	--  UserUnits = ADCValue * fADCToUUFactor + fADCToUUShift;

	type ADCtoUUFactors is record
		scale : ADCtoUUFactor;
		shift : ADC_UserUnits;
	end record;

 	type DAC_UserUnits is new Float;
-- 	type DACtoUUFactors is new ADCtoUUFactors;


	--- define some ranges for bounds checking
	--  types used for "high-level" access
	type ADC_ChannelIndex is new Integer range 1 .. ABF_ADCCOUNT;
	type DAC_ChannelIndex is new Integer range 1 .. ABF_DACCOUNT;

	--  thin binding - zero based, like in ABFHeaderStructure
	--  use PhysicalChannelNum to address its fields..
	subtype ADC_PhysicalChannelIndex is C.size_t range 0 .. ABF_ADCCOUNT - 1;

	---- special array for nADCSamplingSeq
	type SamplingSeqArray is array(ADC_ChannelIndex) of aliased C.short;
	pragma Convention (C, SamplingSeqArray);

	--  DAC index types
	type WaveformIndex is new Integer range 0 .. ABF_WAVEFORMCOUNT - 1;
	type EpochIndex is new Integer range 0 .. ABF_EPOCHCOUNT - 1;


	type AcquisitionMode is (VarLengthEvents, FixedLengthEvents, GapFree, Oscilloscope, EpisodicStimulation);
		--  the corresponfing filed in header starts enumeration at 1 !!, pretty much the only one
		--  That is VarLengthEvents=1, FixedLengthEvents=2, etc.

-- 	type AcqModeNamesArray is array(AcquisitionMode) of String();
-- 	AcqModeNames : constant array(AcquisitionMode) of String :=
-- 		("Variable Length Events", "Fixed Length Events", "Gap Free",
-- 		 "High Speed Oscilloscope", "Episodic Stimulation");

	type ABFDataFormat is (Int16, Float32);
	--  type ChannelScanningMode is (Multiplexed, Simultaneous);
		-- Simultaneous is not supported by Axon yet
		-- just check when reading header


	type    SampleCount is new Integer range 0 .. Integer'Last;
	function "/" (Left : SampleCount; Right : ADC_ChannelIndex) return SampleCount;
	function "*" (Left : SampleCount; Right : ADC_ChannelIndex) return SampleCount;
	function "*" (Left : ADC_ChannelIndex; Right : SampleCount) return SampleCount;

	subtype SampleIndex is SampleCount range 1 .. SampleCount'Last;
		--  used when indexing samples is needed or for related units
	type    BlockCount  is new Integer range 0 .. Integer'Last;
		--  the pointers to sections in file are in "blocks", as stored in header

	type SweepIndex is new Integer range 1 .. Integer'Last;


	--  There are different fields measuring time
	--  we need some separation at least between different time units..
	type TimeBase_sec is new Float;
	type TimeBase_mks is new Float;

	function "*" (Left : SampleCount'Base; Right : TimeBase_mks'Base) return TimeBase_mks'Base;
	function "*" (Left : TimeBase_mks'Base; Right : SampleCount'Base) return TimeBase_mks'Base;
	pragma Inline ("*");

	package TimeBase_IO is new Ada.Text_IO.Float_IO(TimeBase_mks);

	--  main params defining format and usage of parts
	type Mode_Descriptors is record
		acqMode		: AcquisitionMode := GapFree;
		--  numChannels	: ADC_ChannelIndex := 1;

		SampleInterval       : TimeBase_mks := 0.0;
			--  "demultiplexed", = fADCSampleInterval * nADCNumChannels
		SecondSampleInterval : TimeBase_mks := 0.0;
			--  this one is demultiplexed as well
		ClockChangeAt        : SampleCount := 0;
	end record;

end ABF;
