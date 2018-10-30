--
-- Runs some tests of ABT io routines.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--

with Ada.Command_Line, GNAT.Command_Line;
with Ada.Directories, Ada.Environment_Variables;
with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- all methods in .Unbounded have unique names, no need to hide visibility

with ABF.Header.Waveform; -- use ABF.Header; use ABF.Header.Waveform; use ABF;
with ABF.Data; -- use ABF.Data;
with C; use C;
with util; use util;


procedure test_abf_io is
    procedure printUsage is
        use Ada.Text_IO;
    begin
        Put_Line ("Run some general ABF IO tests.");
        Put_Line ("Replace test_data.abf with any of the provided files in dat/. ");
        Put_Line ("If out.atf filename is provided, then contents is also written into the atf file.");
        New_Line;
        Put_Line ("usage:");
        Put_Line ("   " & Ada.Command_Line.Command_Name & " [-h -g -n: -v]  tests_data.abf [out.atf]");
        New_Line;
        Put_Line ("options:");
        --  only short options for now
        Put_Line ("-h      print this help");
        Put_Line ("-g      turn on debug output");
        Put_Line ("-v      be verbose");
        Put_Line ("-n      enter some number");
    end printUsage;

    Finish : exception;
    -- raised in processCommandLine in case of inconsistent parameters passed
    -- serves to skip all further action without producing ugly tracebacks..
    -- (Ada soe not have an analog of sys.exit()  by design. The execution flow is supposed to be explicit)

    type ParamRec is record
        inName, outName : Unbounded_String := Null_Unbounded_String;
        val     : Integer := 0;
        Debug   : Boolean := False;
    end record;

    procedure processCommandLine (params : in out ParamRec) is
        use Ada.Command_Line, GNAT.Command_Line, Ada.Text_IO;
        Options : constant String := "g h v";
        nameCount : Natural := 0;
    begin
        if Argument_Count < 1 then
            printUsage;
            raise Finish;
        end if;
        begin -- need to process local exceptions
            loop
                case Getopt (Options) is
                    when ASCII.NUL =>
                        exit; --  end of option list

                    when 'g' | 'v' => params.Debug := True;
                    when 'h' =>
                        printUsage;
                        raise Finish;

                    when others =>
                        raise Program_Error;
                        --  to catch an option in Options string but not in case statement
                end case;
            end loop;
        exception
            when Invalid_Switch =>
                Put_Line ("Unrecognized option passed: " & Full_Switch);
                raise Finish;
            when Invalid_Parameter =>
                Put_Line ("No parameter for " & Full_Switch);
                raise Finish;
        end;
        -- get positional params
        loop
            declare
                S : constant String := Get_Argument (Do_Expansion => True);
            begin
                exit when S'Length = 0;
                nameCount := nameCount + 1;
                if nameCount = 1 then
                    DebugPrint(params.Debug, "processing file: '" & S &"'");
                    params.inName := To_Unbounded_String(S);
                elsif nameCount = 2 then
                    DebugPrint(params.Debug, "output file: '" & S &"'");
                    params.outName := To_Unbounded_String(S);
                else
                    DebugPrint(params.Debug, "extra names passed, ignoring..");
                end if;
            end;
        end loop;
    end processCommandLine;


    params : ParamRec;

    abfH : ABF.Header.ABFFileHeader;
    wf   : ABF.Header.Waveform.WaveformRec;
    modeDesc : ABF.Mode_Descriptors;
    data : ABF.Data.File_Contents;

    use Ada.Text_IO;

begin
    processCommandLine (params);
    ABF.Debug := params.Debug;
    Put("commencing tests");DebugPrint(params.Debug, " (debug on)", eol=>False);New_Line;
    Put("reading header .. ");
    abfH := ABF.Header.Read_Header(To_String(params.inName));
    Put_Line("done.");
    --
    Put("reading waveform .. ");
    wf := ABF.Header.Waveform.GetWafeform(abfH);
    Put_Line("done.");
    --
    Put("reading mode descriptors .. ");
    modeDesc := ABF.Header.GetModeDescriptors(abfH);
    Put_Line("done.");
    --
    Put("reading data .. ");
    data := ABF.Data.ReadABFFile(To_String(params.inName));
    Put_Line("done.");
    --
    --	Put("ABFFileHeader'Size=");Put(ABFFileHeader'Size/8);New_Line;
    ABF.Header.PrintPrincialParams(abfH);
    --	PrintWaveform(wf);
    --	PrintModeDescriptors(md, File=>Standard_Output);
    -- 	PrintDigiTrainParams(dt, File=>Standard_Output);
    -- 	PrintData(data);
    if params.outName /= Null_Unbounded_String then
        ABF.Data.WriteAtf(To_String(params.outName), data);
--     else
--         WriteAtf("test_data.atf", data);
    end if;
exception
	when Finish => null;
end test_abf_io;
