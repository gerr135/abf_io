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


package body ABF is


function "*" (Left : C.short; Right : ADCtoUUFactor) return ADC_UserUnits is
  begin
	return ADC_UserUnits(Float(Left)*Float(Right));
  end;
  pragma Inline ("*");

function "*" (Left : ADCtoUUFactor; Right : C.short) return ADC_UserUnits is
  begin
	return ADC_UserUnits(Float(Left)*Float(Right));
  end;
  pragma Inline ("*");

function "*" (Left : C.long; Right : ADCtoUUFactor) return ADC_UserUnits is
  begin
	return ADC_UserUnits(Float(Left)*Float(Right));
  end;
  pragma Inline ("*");

function "*" (Left : ADCtoUUFactor; Right : C.long) return ADC_UserUnits is
  begin
	return ADC_UserUnits(Float(Left)*Float(Right));
  end;
  pragma Inline ("*");


function "/" (Left : SampleCount; Right : ADC_ChannelIndex) return SampleCount is
  begin
	return Left / SampleCount(Right);
  end;
  pragma Inline ("/");

function "*" (Left : SampleCount; Right : ADC_ChannelIndex) return SampleCount is
  begin
	return Left * SampleCount(Right);
  end;
  pragma Inline ("*");

function "*" (Left : ADC_ChannelIndex; Right : SampleCount) return SampleCount is
  begin
	return SampleCount(Left) * Right;
  end;
  pragma Inline ("*");


function "*" (Left : SampleCount'Base; Right : TimeBase_mks'Base) return TimeBase_mks'Base is
  begin
  	return TimeBase_mks(Left)*Right;
  end;

function "*" (Left : TimeBase_mks'Base; Right : SampleCount'Base) return TimeBase_mks'Base is
  begin
  	return Left*TimeBase_mks(Right);
  end;

end ABF;