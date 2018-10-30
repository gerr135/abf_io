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

with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

package body ABF.Header.Waveform is

function GetDigiTrain (abfH: in Header.ABFFileHeader) return Digital_Train is
	use C;
  	dt : Digital_Train;
  begin
	dt.Enabled := abfH.nDigitalEnable = 1;
	dt.ActiveDACChannel := DAC_ChannelIndex(abfH.nActiveDACChannel+1);
	dt.DigitalHolding   := abfH.nDigitalHolding;
	dt.DigitalInterEpisode := InterEpisodeHolding'Val(abfH.nDigitalInterEpisode);

	for i in EpochIndex'Range loop
		dt.DigitalValue(i)      := abfH.nDigitalValue(C.size_t(i));
		dt.DigitalTrainValue(i) := abfH.nDigitalTrainValue(C.size_t(i));
	end loop;
	dt.DigitalTrainActiveLogic := DigitalTrainStartPolarity'Val(
		abfH.nDigitalTrainActiveLogic);
	return dt;
  end GetDigiTrain;

procedure PutDigiTrain (abfH : in out Header.ABFFileHeader; dt : in Digital_Train) is
  begin
	if dt.Enabled then
		abfH.nDigitalEnable := 1;
	else
		abfH.nDigitalEnable := 0;
	end if;
	abfH.nActiveDACChannel := C.short(dt.ActiveDACChannel - 1);
	abfH.nDigitalHolding := dt.DigitalHolding;
	abfH.nDigitalInterEpisode := InterEpisodeHolding'Pos(dt.DigitalInterEpisode);

	for i in EpochIndex'Range loop
		abfH.nDigitalValue(C.size_t(i))      := dt.DigitalValue(i);
		abfH.nDigitalTrainValue(C.size_t(i)) := dt.DigitalTrainValue(i);
	end loop;
	abfH.nDigitalTrainActiveLogic :=
		DigitalTrainStartPolarity'Pos(dt.DigitalTrainActiveLogic);
  end PutDigiTrain;

procedure PrintDigiTrain (dg : in Digital_Train;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True) is

	use Ada.Text_IO; use Ada.Integer_Text_IO;

  begin
	if not dg.Enabled then return; end if;
	New_Line(File);
	Put_Line(File, "Digital Train parameters:");
	Put(File, "Enabled: "); Put_Line(File, dg.Enabled'Img);
	Put(File, "Active DAC Channel: "); Put_Line(File, dg.ActiveDACChannel'Img);
	Put(File, "Digital holding: "); Put_Line(File, dg.DigitalHolding'Img);
	Put(File, "Inter Episode:   "); Put_Line(File, dg.DigitalInterEpisode'Img);

	Put(File, "Digital Train:     ");
	for i in EpochIndex'Range loop
		Put(File, Integer(i), Width=>8);
	end loop; New_Line(File);
	Put(File, "DigitalValue:      ");
	for i in EpochIndex'Range loop
		Put(File, Integer(dg.DigitalValue(i)), Width=>8);
	end loop; New_Line(File);
	Put(File, "DigitalTrainValue: ");
	for i in EpochIndex'Range loop
		Put(File, Integer(dg.DigitalTrainValue(i)), Width=>8);
	end loop; New_Line(File);

	Put(File, "DigitalTrainActiveLogic:"); Put_Line(File, dg.DigitalTrainActiveLogic'Img);
  end PrintDigiTrain;



function GetWafeformShapes (abfH: in Header.ABFFileHeader) return Waveform_Shapes is
	use C;
	wf: Waveform_Shapes;
  begin
	for wfCount in WaveformIndex'Range loop
		--Put("wfCount=");Put(wfCount'Img);Put(", ");
		wf.DACEnabled(wfCount) := abfH.nWaveformEnable(C.size_t(wfCount)) = 1;
		wf.WaveformSource(wfCount) :=
			wfSource'Val(abfH.nWaveformSource(C.size_t(wfCount)));
		wf.InterEpisodeLevel(wfCount) :=
			InterEpisodeHolding'Val(abfH.nInterEpisodeLevel(C.size_t(wfCount)));
		-- epoch definitions
		for epochCount in EpochIndex'Range loop
			wf.EpochType(wfCount)(epochCount) :=
				EpochTypes'Val(abfH.nEpochType(C.size_t(wfCount),C.size_t(epochCount)));

			wf.EpochInitLevel(wfCount)(epochCount) := DAC_UserUnits(
				abfH.fEpochInitLevel(C.size_t(wfCount),C.size_t(epochCount)) );
			wf.EpochLevelInc(wfCount)(epochCount) := DAC_UserUnits(
				abfH.fEpochLevelInc(C.size_t(wfCount),C.size_t(epochCount)) );

			wf.EpochInitDuration(wfCount)(epochCount) := SampleCount(
				abfH.lEpochInitDuration(C.size_t(wfCount),C.size_t(epochCount)) );
			wf.EpochDurationInc(wfCount)(epochCount) := SampleCount(
				abfH.lEpochDurationInc(C.size_t(wfCount),C.size_t(epochCount)) );
		end loop;  -- EpochIndex'Range
	end loop; -- WaveformIndex'Range
  	return wf;
  end GetWafeformShapes;

procedure PutWafeformShapes (abfH : in out Header.ABFFileHeader; wf : in Waveform_Shapes) is
  begin
	DACChannels:
	for wfCount in WaveformIndex'Range loop
		if wf.DACEnabled(wfCount) then
			abfH.nWaveformEnable(C.size_t(wfCount)) := 1;
		else
			abfH.nWaveformEnable(C.size_t(wfCount)) := 0;
		end if;
		abfH.nWaveformSource(C.size_t(wfCount)) := wfSource'Pos(wf.WaveformSource(wfCount));
		abfH.nInterEpisodeLevel(C.size_t(wfCount)) :=
			InterEpisodeHolding'Pos(wf.InterEpisodeLevel(wfCount));

		-- epoch definitions
		Epochs:
		for epochCount in EpochIndex'Range loop
			abfH.nEpochType(C.size_t(wfCount),C.size_t(epochCount)) :=
				EpochTypes'Pos(wf.EpochType(wfCount)(epochCount));

			abfH.fEpochInitLevel(C.size_t(wfCount),C.size_t(epochCount)) :=
				C.C_float(wf.EpochInitLevel(wfCount)(epochCount));
			abfH.fEpochLevelInc(C.size_t(wfCount),C.size_t(epochCount))  :=
				C.C_float(wf.EpochLevelInc(wfCount)(epochCount));

			abfH.lEpochInitDuration(C.size_t(wfCount),C.size_t(epochCount)) :=
				C.long(wf.EpochInitDuration(wfCount)(epochCount));
			abfH.lEpochDurationInc(C.size_t(wfCount),C.size_t(epochCount))  :=
				C.long(wf.EpochDurationInc(wfCount)(epochCount));
		end loop Epochs;
	end loop DACChannels;
  end PutWafeformShapes;


procedure PrintWaveformShapes (wf : in Waveform_Shapes;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True) is

	use Ada.Text_IO;use Ada.Float_Text_IO;use Ada.Integer_Text_IO;

  begin
  	New_Line(File);
  	Put_Line(File, "Episodes:");
  	for wfCount in WaveformIndex'Range loop
  		if wf.DACEnabled(wfCount) then
			Put(File,"wfCount = ");Put_Line(File,wfCount'Img);

			if not Only_Defined then
				Put(File,"  DAC Enabled=");
				Put(File,wf.DACEnabled(wfCount)'Img);
				Put(File, ",  ");
			end if;
			Put(File,"wfSource=");Put(File,wf.WaveformSource(wfCount)'Img);
			Put(File,",  ieLevel=");Put(File,wf.InterEpisodeLevel(wfCount)'Img);
			New_Line(File);

			-- epochs
			Put(File,"epoch num:  ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File, Integer(epochCount), Width=>8);
			end loop; New_Line(File);

			Put(File,"epoch type: ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File,wf.EpochType(wfCount)(epochCount)'Img);
				Put(File,"  ");
			end loop; New_Line(File);

			Put(File,"init level: ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File,Float(wf.EpochInitLevel(wfCount)(epochCount)), Fore=>5, Aft=>2, Exp=>0);
			end loop; New_Line(File);

			Put(File,"level inc:  ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File,Float(wf.EpochLevelInc(wfCount)(epochCount)), Fore=>5, Aft=>2, Exp=>0);
			end loop; New_Line(File);

			Put(File,"init durat: ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File,Integer(wf.EpochInitDuration(wfCount)(epochCount)),Width=>8);
			end loop; New_Line(File);

			Put(File,"durat inc:  ");
			for epochCount in EpochIndex'Range loop
				exit when wf.EpochType(wfCount)(epochCount) = Disabled;
				Put(File,Integer(wf.EpochDurationInc(wfCount)(epochCount)),Width=>8);
			end loop; New_Line(File);

			New_Line(File);
		end if;
	end loop; -- WaveformIndex'Range
  end PrintWaveformShapes;

function  GetWafeform   (abfH : in Header.ABFFileHeader) return WaveformRec is
	use C;
	wf: WaveformRec;
  begin
  	--  first some consistency checks
	if -- (abfH.lEpisodesPerRun /= abfH.lActualEpisodes) or
	   (abfH.lNumberOfTrials /= 1) or
	   (abfH.nFirstEpisodeInRun /= 0)
	   then
		raise Inconsistent_Header_Params;
	end if;

	wf.numRuns     := Natural(abfH.lRunsPerTrial);
	wf.numEpisodes := SweepIndex(abfH.lActualEpisodes);

	wf.NumSamplesPerEpisode := SampleCount(abfH.lNumSamplesPerEpisode);
	wf.EpisodeStartToStart  := TimeBase_sec(abfH.fEpisodeStartToStart);
	wf.RunStartToStart      := TimeBase_sec(abfH.fRunStartToStart);

  	wf.digiTrain := GetDigiTrain(abfH);
  	wf.wfShapes  := GetWafeformShapes(abfH);
  	return wf;
  end GetWafeform;

procedure PutWaveform (abfH : in out Header.ABFFileHeader; wf : in WaveformRec) is
  begin
	abfH.lNumberOfTrials    := 1;
	abfH.nFirstEpisodeInRun := 0;

	abfH.lRunsPerTrial   := C.long(wf.numRuns);
	abfH.lActualEpisodes := C.long(wf.numEpisodes);
	abfH.lNumSamplesPerEpisode := C.long(wf.NumSamplesPerEpisode);
	abfH.fEpisodeStartToStart  := C.C_float(wf.EpisodeStartToStart);
	abfH.fRunStartToStart := C.C_float(wf.RunStartToStart);

  	PutDigiTrain(abfH, wf.digiTrain);
  	PutWafeformShapes(abfH, wf.wfShapes);
  end PutWaveform;


procedure PrintWaveform (wf : in WaveformRec;
			File : in Text_IO.File_Type := Text_IO.Standard_Output;
			Only_Defined : Boolean := True) is

	use Ada.Text_IO;use Ada.Float_Text_IO;use Ada.Integer_Text_IO;

  begin
  	New_Line(File); Put_Line(File, "Waveform display:");
  	Put(File, "runs:     ");Put(File, wf.numRuns'Img);
  	Put(File, ",  episodes: ");Put(File, wf.numEpisodes'Img);
  	New_Line(File);

  	Put(File, "# samples per episode: ");
  	Put(File, wf.NumSamplesPerEpisode'Img);New_Line(File);

  	Put(File, "time, interEpisode: ");Put(File, Float(wf.EpisodeStartToStart), Exp => 0);
  	Put(File, ", interRun: ");Put(File, Float(wf.RunStartToStart), Exp => 0);
	New_Line(File);

  	PrintWaveformShapes(wf.wfShapes, File, Only_Defined);
  	PrintDigiTrain(wf.digiTrain, File, Only_Defined);
  end PrintWaveform;


end ABF.Header.Waveform;
