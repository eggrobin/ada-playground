with Ada.Long_Long_Integer_Text_IO;
with Ada.Text_IO;

procedure Ada_Playground is
   type Block is range -1 .. Integer'Last;
   Empty_Block : constant Block := -1;
   subtype File_ID is Block range 0 .. Block'Last;

   type Block_Position is new Natural;
   subtype Map_Size is Block_Position range 0 .. 9;
   subtype File_Size is Map_Size range 1 .. 9;
   type File_Layout is
      record
         Size                : File_Size;
         Trailing_Free_Space : Map_Size;
      end record;

   type Disk_Map is array (File_ID range <>) of File_Layout;

   type Blocks is array (Block_Position range <>) of Block;
   Empty_Disk_Map : constant Disk_Map (0 .. -1) := (others => (1, 0));

   package Map_Size_IO is new Ada.Text_IO.Integer_IO (Map_Size);
   package Block_IO is new Ada.Text_IO.Integer_IO (Block);

   procedure Put_Disk_If_Small (Disk : Blocks) is
   begin
      if Disk'Length < 80 then
         for B of Disk loop
            if B = Empty_Block then
               Ada.Text_IO.Put ('.');
            else
               Block_IO.Put (B, Width => 1);
            end if;
         end loop;
         Ada.Text_IO.New_Line;
      end if;
   end Put_Disk_If_Small;

   function Read_Disk_Map
      (File         : Ada.Text_IO.File_Type;
       Already_Read : Disk_Map := Empty_Disk_Map) return Disk_Map is
      Result : Disk_Map (Already_Read'First ..
                         Already_Read'First + 2 * Already_Read'Length);
   begin
      for I in Already_Read'Range loop
         Result (I) := Already_Read (I);
      end loop;
      for I in Already_Read'Last + 1 .. Result'Last loop
         if Ada.Text_IO.End_Of_File (File) then
            return Result (Result'First .. I - 1);
         end if;
         Map_Size_IO.Get (File, Result (I).Size,
                          Width => 1);
         if Ada.Text_IO.End_Of_File (File) then
            Result (I).Trailing_Free_Space := 0;
         else
            Map_Size_IO.Get (File, Result (I).Trailing_Free_Space,
                             Width => 1);
         end if;
      end loop;
      return Read_Disk_Map (File, Already_Read => Result);
   end Read_Disk_Map;

   function Mapped_Blocks (Map : Disk_Map) return Blocks is
      Block_Count : Block_Position := 0;
   begin
      for File of Map loop
         Block_Count := Block_Count + File.Size;
         Block_Count := Block_Count + File.Trailing_Free_Space;
      end loop;
      declare
         Result     : Blocks (0 .. Block_Count - 1);
         Next_Block : Block_Position := 0;
      begin
         for File_ID in Map'Range loop
            Result (Next_Block ..
                    Next_Block + Map (File_ID).Size - 1) :=
               (others => File_ID);
            Next_Block := Next_Block + Map (File_ID).Size;
            Result (Next_Block ..
                    Next_Block + Map (File_ID).Trailing_Free_Space - 1) :=
               (others => Empty_Block);
            Next_Block := Next_Block + Map (File_ID).Trailing_Free_Space;
         end loop;
         return Result;
      end;
   end Mapped_Blocks;

   procedure Fragment (Disk : in out Blocks) is
      Next_Empty    : Block_Position := Disk'First;
      Block_To_Move : Block_Position := Disk'Last;
   begin
      loop
         Put_Disk_If_Small (Disk);
         while Disk (Next_Empty) /= Empty_Block loop
            Next_Empty := Next_Empty + 1;
         end loop;
         while Disk (Block_To_Move) = Empty_Block loop
            Block_To_Move := Block_To_Move - 1;
         end loop;
         exit when Block_To_Move < Next_Empty;
         Disk (Next_Empty)    := Disk (Block_To_Move);
         Disk (Block_To_Move) := Empty_Block;
      end loop;
   end Fragment;

   function Packed_Files (Map : Disk_Map) return Blocks is
      Disk : Blocks := Mapped_Blocks (Map);
   begin
      for File_To_Move in reverse Map'Range loop
         Put_Disk_If_Small (Disk);
         declare
            File_Location : Block_Position := Disk'First;
            Candidate     : Block_Position := Disk'First;
         begin
            while Disk (File_Location) /= File_To_Move loop
               File_Location := File_Location + 1;
            end loop;
            loop
               while Disk (Candidate) /= Empty_Block loop
                  Candidate := Candidate + 1;
               end loop;
               exit when Candidate > File_Location;
               for B in Candidate ..
                        Candidate + Map (File_To_Move).Size - 1 loop
                  if Disk (B) /= Empty_Block then
                     Candidate := B + 1;
                     goto Next_Candidate;
                  end if;
               end loop;
               Disk (Candidate .. Candidate + Map (File_To_Move).Size - 1) :=
                  Disk (File_Location ..
                        File_Location + Map (File_To_Move).Size - 1);
               Disk (File_Location ..
                     File_Location + Map (File_To_Move).Size - 1) :=
                  (others => Empty_Block);
               exit;
               <<Next_Candidate>>
            end loop;
         end;
      end loop;
      Put_Disk_If_Small (Disk);
      return Disk;
   end Packed_Files;

   function Checksum (Disk : Blocks) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      for Block_Position in Disk'Range loop
         if Disk (Block_Position) /= Empty_Block then
            Result := Result +
               Long_Long_Integer (Block_Position) *
                  Long_Long_Integer (Disk (Block_Position));
         end if;
      end loop;
      return Result;
   end Checksum;

   Input : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, "input_short.txt");
   declare
      Map             : constant Disk_Map := Read_Disk_Map (Input);
      Fragmented_Disk : Blocks            := Mapped_Blocks (Map);
   begin
      Fragment (Fragmented_Disk);
      Ada.Text_IO.Put ("Checksum after fragmenting compactification: ");
      Ada.Long_Long_Integer_Text_IO.Put (Checksum (Fragmented_Disk));
      Ada.Text_IO.New_Line;
      declare
         Non_Fragmented_Disk : constant Blocks := Packed_Files (Map);
      begin
         Ada.Text_IO.Put ("Checksum after non-fragmenting compactification: ");
         Ada.Long_Long_Integer_Text_IO.Put (Checksum (Non_Fragmented_Disk));
         Ada.Text_IO.New_Line;
      end;
   end;
end Ada_Playground;
