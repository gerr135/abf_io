--**************************************************************************
--   Copyright (C) 2005 by George Shapovalov  <gshapovalov@gmail.com>     --
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
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with util; use util;

package body ABF.Header is

function  Empty_GapFree_ABFFileHeader return ABFFileHeader is Separate;

function PhysicalChannelNum(abfH : Header.ABFFileHeader; nChan : ADC_ChannelIndex)
		return ADC_PhysicalChannelIndex is
begin
	return ADC_PhysicalChannelIndex(abfH.nADCPtoLChannelMap(
		ADC_PhysicalChannelIndex(abfH.nADCSamplingSeq(nChan))));
end;

procedure PrintPrincialParams(abfH: in ABFFileHeader;
    File : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output) is
    use Ada.Float_Text_IO; use Ada.Integer_Text_IO;
    begin
        New_Line(File);
        Put_Line(File,"contents of the file header:");

        Put(File,"file sig: ");Put_Line(File,abfH.lFileSignature'Img);
        Put(File,"version:    ");Put_Line(File,abfH.fFileVersionNumber'Img);
        Put(File,"header ver: ");Put_Line(File,abfH.fHeaderVersionNumber'Img);
        Put(File,"file type:  ");Put_Line(File,abfH.nFileType'Img);
        Put(File,"operation mode:  ");Put_Line(File,abfH.nOperationMode'Img);
        Put(File,"experiment type: ");Put_Line(File,abfH.nExperimentType'Img);
        New_Line(File);

        Put(File,"scopes");New_Line(File);
        Put(File,"num scopes: ");Put_Line(File,abfH.lNumScopes'Img);
        Put(File,"scope starts: ");Put_Line(File,abfH.lScopeConfigPtr'Img);
        New_Line(File);

        Put(File,"data");New_Line(File);
        Put(File,"data format: ");Put_Line(File,abfH.nDataFormat'Img);
        Put(File,"data starts at:   ");Put_Line(File,abfH. lDataSectionPtr'Img);
        Put(File,"ignored at start: ");Put_Line(File,abfH.nNumPointsIgnored'Img);
        Put(File,"ack length: ");Put_Line(File,abfH.lActualAcqLength'Img);
        Put(File,"num channels, sampled: ");Put(File,abfH.nADCNumChannels'Img);
        Put(File, ",   acquired:");Put_Line(File,abfH.channel_count_acquired'Img);
        Put(File,"sample intevals, 1st: " );
        Put(File, Float(abfH.fADCSampleInterval), Aft => 0, Exp => 0);
        Put(File, ", 2nd: ");
        Put(File, Float(abfH.fADCSecondSampleInterval), Aft => 0, Exp => 0);
        New_Line(File);
        Put(File,"requested length: ");
        Put(File, Float(abfH.fSecondsPerRun), Aft => 3, Exp => 0);
        New_Line(File);
        Put(File,"simult scan: ");Put_Line(File,abfH.nSimultaneousScan'Img);
        New_Line(File);

        Put_Line("waveform:");
        Put(File,"requested num sweeps: ");Put(File,abfH.lEpisodesPerRun'Img);
        Put(File,",   num sweeps: ");Put_Line(File,abfH.lActualEpisodes'Img);
        Put(File,"requested num runs:   ");Put_Line(File,abfH.lRunsPerTrial'Img);
        Put(File,"requested num trials:  ");Put_Line(File,abfH.lNumberOfTrials'Img);
        Put(File,"averaging: ");Put_Line(File,abfH.nAveragingMode'Img);
        Put(File,"1st episode in run:");Put_Line(File,abfH.nFirstEpisodeInRun'Img);
        Put(File,"num samples/sweep:");Put_Line(File,abfH.lNumSamplesPerEpisode'Img);

        New_Line(File);
        Put(File,"gain-offset");New_Line(File);
        Put(File,"ADC range: ");Put_Line(File,abfH.fADCRange'Img);
        Put(File,"ADC resolution: ");Put_Line(File,abfH.lADCResolution'Img);
        Put(File,"DAC range: ");Put_Line(File,abfH.fDACRange'Img);
        Put(File,"DAC resolution: ");Put_Line(File,abfH.lDACResolution'Img);

        New_Line(File);
        Put(File,"cnahhels:     ");
        for i in 0 .. ABF_ADCCOUNT-1 loop
            Put(File,i,Width=>8); 
        end loop;
        New_Line(File);
        Put(File,"P2L chan map: ");
        for i in abfH.nADCPtoLChannelMap'Range loop
            Put(File,Integer(abfH.nADCPtoLChannelMap(i)),Width=>8);
        end loop;
        New_Line(File);
        Put(File,"sampling seq: ");
        for i in abfH.nADCSamplingSeq'Range loop
            Put(File,Integer(abfH.nADCSamplingSeq(i)),Width=>8);
        end loop;
        New_Line(File);
        --
        -- 	char  chname[10];
        -- 	Put_Line(File,"\nchannl names: ");for (i=0;i<numChan;i++){strncpy(chname,abfH.sDACChannelName[i], ABF_DACNAMELEN);Put_Line(File,"%8s",&chname);};
        -- 	char chUnits[8];
        -- 	Put_Line(File,"\nchannl units: ");for (i=0;i<numChan;i++){GetADCChannelUnits(pH,i,chUnits);Put_Line(File,"%8s",&chUnits);};
        -- 	//Put_Line(File,"\nchannl units all: %s",abfH.sADCUnits);
        --
        -- 	Put_Line(File,"\nprogram gain: ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fADCProgrammableGain[i]);
        -- 	Put_Line(File,"\ninstr scale:  ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fInstrumentScaleFactor[i]);
        -- 	Put_Line(File,"\nsignal gain:  ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fSignalGain[i]);
        -- 	Put_Line(File,"\ncumulat gain: ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fADCProgrammableGain[i]*abfH.fInstrumentScaleFactor[i]*abfH.fSignalGain[i]);
        --
        -- 	Put_Line(File,"\n\ninstr offset: ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fInstrumentOffset[i]);
        -- 	Put_Line(File,"\nsignal offs:  ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fSignalOffset[i]);
        -- 	Put_Line(File,"\ncumulat offs: ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fInstrumentOffset[i]+abfH.fSignalOffset[i]);
        --
        -- 	Put_Line(File,"\n\nlowpass flt:  ");for (i=0;i<numChan;i++)Put_Line(File,"%8.0f",abfH.fSignalLowpassFilter[i]);
        -- 	Put_Line(File,"\nhipass flt:   ");for (i=0;i<numChan;i++)Put_Line(File,"%8.3f",abfH.fSignalHighpassFilter[i]);
        -- 	Put_Line(File,"\n\n");
    end;


procedure Write_Header (abfH: in ABFFileHeader; FileName: String) is
    File : Stream_IO.File_Type;
    File_Access : Stream_IO.Stream_Access;
    begin
        begin
            Stream_IO.Open(File => File, Mode=>Stream_IO.Out_File, Name => FileName);
        exception
            when Stream_IO.Name_Error =>
            Stream_IO.Create(File => File, Mode=>Stream_IO.Out_File, Name => FileName);
        end;
        File_Access := Stream_IO.Stream(File => File);
        ABFFileHeader'Write(File_Access, abfH);
        Stream_IO.Close(File);
    end;

function  Read_Header  (FileName: String) return ABFFileHeader is
    File : Stream_IO.File_Type;
    File_Access : Stream_IO.Stream_Access;
    abfH : ABFFileHeader;
    begin
        Stream_IO.Open(File => File, Mode=>Stream_IO.In_File, Name => FileName);
        File_Access := Stream_IO.Stream(File => File);
        ABFFileHeader'Read(File_Access, abfH);
        Stream_IO.Close(File);

        -- do some consistency checks
        if (abfH.lFileSignature /= ABF_NATIVESIGNATURE) or
            (abfH.lHeaderSize    /= ABF_HEADERSIZE)     or
            ((abfH.nOperationMode = ABF_GAPFREEFILE)    and
            (-- (abfH.lActualEpisodes /= 1) or -- Axon does not seem to follow its own guide!!!
            (abfH.fADCSecondSampleInterval /= 0.0)) )
        then
            DebugPrint(Debug, "");
            DebugPrint(Debug, "file sig:     '" & CLong2String(abfH.lFileSignature) & "',  expected:  '" & CLong2String(ABF_NATIVESIGNATURE) & "'");
            DebugPrint(Debug, "header size:  " & abfH.lHeaderSize'Img & ",  expected:  " & ABF_HEADERSIZE'Img);
            DebugPrint(Debug, "acq mode:     " & abfH.nOperationMode'Img & ",  expected:  " & ABF_GAPFREEFILE'Img);
            DebugPrint(Debug, "num episodes: " & abfH.lActualEpisodes'Img);
            DebugPrint(Debug, "2nd sample:   " & abfH.fADCSecondSampleInterval'Img & ",  expected:  0.0");
            raise Inconsistent_Header_Params;
        end if;

        if (abfH.nFileType /= ABF_ABFFILE) or
        (abfH.nMSBinFormat /= 0) or
        (abfH.nSimultaneousScan /= 0)
        then
            raise Unsupported_Header_Params;
        end if;

        return abfH;
    end;

---------------------------------------------------
--  data handling, scaling factors, waveforms, etc.
---------------------------------------------------

--  Calculates the scaling factors used to convert DAC values to UserUnits.
--   nChannel  - The physical channel number to get the factors for.
--   returns scale and offset. UserUnits = DACValue * fDACToUUFactor + fDACToUUShift;
function  GetADCtoUUFactors(abfH : in ABFFileHeader;
    nChannel : in ADC_ChannelIndex) return ADCtoUUFactors is

    factors : ADCtoUUFactors;
    nChn : ADC_PhysicalChannelIndex := PhysicalChannelNum(abfH, nChannel);

    TotalScaleFactor : C.C_float := abfH.fInstrumentScaleFactor(nChn) *
                                    abfH.fADCProgrammableGain(nChn);
    -- InputRange and InputOffset is the range and offset of the signal in
    -- user units when it hits the Analog-to-Digital converter
    InputRange, InputOffset : C.C_float;

    begin
        if abfH.nSignalType /= 0 then
            TotalScaleFactor := TotalScaleFactor * abfH.fSignalGain(nChn);
        end if;

        -- Adjust for the telegraphed gain.
        if abfH.nTelegraphEnable(nChn) /= 0 then
            TotalScaleFactor := TotalScaleFactor * abfH.fTelegraphAdditGain(nChn);
        end if;
        -- 	Put("TotalSF=");Put_Line(TotalScaleFactor'Img);
        -- 	Put("InstrScale=");Put_Line(abfH.fInstrumentScaleFactor(nChn)'Img);
        -- 	Put("Prog gain =");Put_Line(abfH.fADCProgrammableGain(nChn)'Img);
        -- 	Put("nChn =");Put_Line(nChn'Img);

        if TotalScaleFactor = 0.0 then TotalScaleFactor := 1.0; end if;

        InputRange  :=  abfH.fADCRange / TotalScaleFactor;
        InputOffset := -abfH.fInstrumentOffset(nChn);
        -- 	Put("IRange=");Put_Line(InputRange'Img);
        -- 	Put("IOffst=");Put_Line(InputOffset'Img);
        -- 	New_Line;

        if abfH.nSignalType /= 0 then
            InputOffset := InputOffset + abfH.fSignalOffset(nChn);
        end if;

        factors.scale := ADCtoUUFactor(InputRange / C.C_float(abfH.lADCResolution));
        factors.shift := ADC_UserUnits(-InputOffset);

        --  Above was done following axon's code, but to get values really corresponding
        --  to UserUnits, (and as stored in Float32 version of abf files)
        --  this needs to be divided by 2*ADCRange

        -- 	factors.scale := factors.scale / ADCtoUUFactor(2.0*abfH.fADCRange);
        -- 	factors.shift := factors.shift / ADC_UserUnits(2.0*abfH.fADCRange);
        return factors;
    end;

procedure ClipADCUUValue(abfH : in ABFFileHeader;
    nChannel : in ADC_ChannelIndex; value : in out ADC_UserUnits) is

    factors : ADCtoUUFactors := GetADCtoUUFactors(abfH, nChannel);
    UUMax : ADC_UserUnits := (abfH.lADCResolution - 1) * factors.scale + factors.shift;
    UUMin : ADC_UserUnits := (  - abfH.lADCResolution) * factors.scale + factors.shift;
    swapTmp : ADC_UserUnits;
    begin
        if UUMax < UUMin then
            swapTmp := UUMax; UUMax := UUMin; UUMin := swapTmp;
        end if;
        if value > UUMax then value := UUMax; end if;
        if value < UUMin then value := UUMin; end if;
    end;

-- function GetDADCtoUUFactors(abfH : in ABFFileHeader;
-- 		nChannel : ADC_ChannelIndex) return DACtoUUFactors is
-- 	factors : DACtoUUFactors;
--   begin
-- 	return factors;
--   end;


-- procedure ClipDACUUValue(abfH : in ABFFileHeader;
-- 		nChannel : in ADC_ChannelIndex; value : in out Float) is
--   begin
-- 	Null;
--   end;



function  GetModeDescriptors (abfH : in Header.ABFFileHeader) return Mode_Descriptors is
    md : Mode_Descriptors;
    begin
        md.acqMode := AcquisitionMode'Val(abfH.nOperationMode - 1);
        --  md.numChannels := ADC_ChannelIndex(abfH.nADCNumChannels);
        md.SampleInterval := TimeBase_mks(abfH.fADCSampleInterval) * TimeBase_mks(abfH.nADCNumChannels);
        md.SecondSampleInterval := TimeBase_mks(abfH.fADCSecondSampleInterval)
                * TimeBase_mks(abfH.nADCNumChannels);
        md.ClockChangeAt := SampleCount(abfH.lClockChange);
        return md;
    end;

procedure PrintModeDescriptors (md : in Mode_Descriptors; File : in Ada.Text_IO.File_Type) is
    begin
        New_Line(File);
        Put_Line(File, "Mode descriptors:");
        Put(File, "acq mode: "); Put_Line(File, md.acqMode'Img);
        --	Put(File, "num chan: "); Put_Line(File, md.numChannels'Img);
        Put(File, "dt1: "); Put(File, md.SampleInterval'Img);
        Put(File, ",  dt2: "); Put(File, md.SecondSampleInterval'Img);
        Put(File, ",  change at: "); Put_Line(File, md.ClockChangeAt'Img);
    end;


--  inline ABFFileHeader::ABFFileHeader()
--  {
--   Set everything to 0.
--  memset( this, 0, sizeof(ABFFileHeader) );
--
--   Set critical parameters so we can determine the version.
--  lFileSignature       = ABF_NATIVESIGNATURE;
--  fFileVersionNumber   = ABF_CURRENTVERSION;
--  fHeaderVersionNumber = ABF_CURRENTVERSION;
--  lHeaderSize          = ABF_HEADERSIZE;
--  }

end ABF.Header;
