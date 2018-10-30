--
--  Contains needed x86 32bit types definitions (as used in original ABF spec)
--
--  Author: George Shapovalov <gshapovalov@gmail.com>, (C) 2005, 2018
--
--  Copyright: See COPYING file that comes with this distribution
--

with Interfaces;

package C is
pragma Pure (C);

	--  abf format uses x86 32bit types.
	--  define relevant ones here, so there is no confusion

	--  Signed and Unsigned Integers.

	type int   is new Interfaces.Integer_32;
	type short is new Interfaces.Integer_16;
	type long  is new Interfaces.Integer_32;

-- 	type signed_char is range SCHAR_MIN .. SCHAR_MAX;
-- 	for signed_char'Size use CHAR_BIT;

	type unsigned       is mod 2 ** int'Size;
	type unsigned_short is mod 2 ** short'Size;
	type unsigned_long  is mod 2 ** long'Size;
--
-- 	type unsigned_char is mod (UCHAR_MAX + 1);
-- 	for unsigned_char'Size use CHAR_BIT;

	type size_t is mod 2 ** Standard'Address_Size;


--  Floating-Point ----------------------------------

	type C_float     is new Interfaces.IEEE_Float_32;
	type double      is new Interfaces.IEEE_Float_64;
--	type long_double is new Standard.Long_Long_Float;


-- Characters and Strings ---------------------------

	CHAR_BIT : constant := 8;

	type char is new Character;

	type char_array is array (size_t range <>) of aliased char;
	for char_array'Component_Size use CHAR_BIT;


--  Necessary arrays ---------------------------------

	type shortArray is array (C.size_t range <>) of aliased C.short;
    pragma Convention (C, shortArray);
	type longArray  is array (C.size_t range <>) of aliased C.long;
	pragma Convention (C, longArray);
	type floatArray is array (C.size_t range <>) of aliased C.C_float;
	pragma Convention (C, floatArray);

	type char2DArray  is array (C.size_t range <>, C.size_t range <>) of aliased C.char;
	pragma Convention (C, char2DArray);
	type short2DArray is array (C.size_t range <>, C.size_t range <>) of aliased C.short;
	pragma Convention (C, short2DArray);
	type long2DArray  is array (C.size_t range <>, C.size_t range <>) of aliased C.long;
	pragma Convention (C, long2DArray);
	type float2DArray is array (C.size_t range <>, C.size_t range <>) of aliased C.C_float;
	pragma Convention (C, float2DArray);


end C;
