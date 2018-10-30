--
--
--  Ada Spec: ldt_io.ads
--
--  Description: Structure and IO of .ldt files (as produced bu QuB)
--
--  Author: George Shapovalov <gshapovalov@gmail.com>, (C) 2007
--
--  Copyright: See COPYING file that comes with this distribution
--
--

--with Ada.Text_IO;
with Ada.Streams.Stream_IO;
--with Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

package LDT_IO is

	---------------------------------------
	--  Some exceptions and notifications
	Not_Implemented : exception;


	--  Should perhaps be a child of ABF (or that renamed as a common ancestor)
	--  as that one has some usefull types for time and sample counting as well as 
	--  generic file IO stuff defined.
	--
	--  For now simply a few types that define .ldt structure
	--  The format is very basic - contains a header and segments of data.
	--
	--  <LDT> = (
	--  	<ldt_head>
	--  	<ldt_seg>*
	--  )
	--
	--  <ldt_seg> =  (
	--  	<ldt_seg_head>
	--  	<ldt_seg_body>
	--  )


	type ldt_head is record
		PadSize : Integer_32;  -- N datapoint before and after the segment
		dt      : Unsigned_16; -- sampling interval in mks
		scaling : Unsigned_16; -- scaling factor
	end record;
	
	type ldt_seg_head is record;
		start : Integer_32; -- start time in mks
		Length : Integer_32; -- length of the segment (in points)
	end record;

	type ldt_seg_body is array(Integer_32 range <>) of Integer_16;

private


end ABF;
