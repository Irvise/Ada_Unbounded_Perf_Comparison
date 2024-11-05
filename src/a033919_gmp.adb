--  Rosetta Code Task written in Ada
--  Smallest number k such that k+2^m is composite for all m less than k
--  https://rosettacode.org/wiki/Smallest_number_k_such_that_k%2B2%5Em_is_composite_for_all_m_less_than_k
--  loosely translated from the Python solution
--  November 2024, R. B. E. and Fernando Oleo Blanco

pragma Ada_2022;
pragma Debug_Policy (On);

with Ada.Text_IO; -- use Ada.Text_IO;
with Ada.Integer_Text_IO; -- use Ada.Integer_Text_IO;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP; use GNATCOLL.GMP;

procedure A033919_GMP
  with SPARK_Mode => On
is
   Big_0 : constant Big_Integer := Make ("0");
   Big_2 : constant Big_Integer := Make ("2");
   Big_3 : constant Big_Integer := Make ("3");
   Big_4 : constant Big_Integer := Make ("4");
   Big_5 : constant Big_Integer := Make ("5");
   Big_7 : constant Big_Integer := Make ("7");

   function Is_Prime (N : Big_Integer) return Boolean
     with Inline
   is
      Big_Temp : Big_Integer;
      Counter : Long_Long_Long_Integer := 1;
   begin

      if N mod Big_2 = Big_0 then
         return N = Big_2;
      end if;

      if N mod Big_3 = Big_0 then
         return N = Big_3;
      end if;

      if N mod Big_5 = Big_0 then
         return N = Big_5;
      end if;

      Set (Big_Temp, Big_7);
      pragma Debug (Ada.Text_IO.Put_Line ("Testing for prime " & Image (N)));
      while Big_Temp * Big_Temp <= N loop
         if N mod Big_Temp = Big_0 then
            return False;
         end if;
         Set (Big_Temp, Big_Temp + Big_2);
         Counter := Counter + 1;
         if Counter mod 10_000_000 = 0 then
            pragma Debug (Ada.Text_IO.Put_Line ("Current Big_Temp value is " & Image(Big_Temp)));
         end if;
      end loop;
      return True;

   end Is_Prime;

   subtype Big_Prime is Big_Integer
     with Dynamic_Predicate => Is_Prime (Big_Prime);

   --  The following subtype is not valid as Big_Positive cannot be used in the `for all` range
   --  subtype Big_Prime is Big_Positive
   --     with Dynamic_Predicate => (for all Divisor in 2 .. Big_Prime / 2 => Big_Prime mod Divisor /= 0);

   Upper_Table_Bound : constant Positive := 5200; -- R.B.E: 5200 is just a rough upper limit guess of how many I need
                                                  -- Irvise: this bound will not be enough for the larger numbers.

   type Powers_of_Two_Table is array (Positive'First .. Upper_Table_Bound) of Big_Integer; -- Changed based index from 0 to 1, that simplifies things
                                                                                           --     with Dynamic_Predicate => (for all I in Powers_Of_Two_Table'Range => Powers_Of_Two_Table (I) = Raise_To_N (Big_2, Unsigned_Long (I)));

   Powers_of_Two : Powers_of_Two_Table; -- We initialise the table and make it constant

   --  Taken from Simon's GMP solution
   procedure Load_Powers_of_Two (Powers_of_Two : in out Powers_of_Two_Table) is
   begin
      for I in Powers_of_Two'Range loop
         if I = 1 then
            Set (Powers_of_Two (I), To => Big_2);
         elsif I = 2 then
            Set (Powers_of_Two (I), To => Big_4);
         else
            Multiply
              (Result => Powers_of_Two (I), Op1 => Powers_of_Two (I - 1),
               Op2    => Big_2);
         end if;
      end loop;
   end Load_Powers_of_Two;

   procedure Show_Powers_of_Two (Powers_of_Two : Powers_of_Two_Table) is
   begin
      for I of Powers_of_Two loop
         Ada.Text_IO.Put_Line (I'Image);
      end loop;
   end Show_Powers_of_Two;

   function Is_A033919 (K : Positive) return Boolean
     with Inline
   is
      N : Big_Integer;
      M : Positive := Powers_of_Two_Table'First;
   begin
      while M < K and then M < Powers_of_Two_Table'Last loop
         Set (N, Powers_of_Two (M) + Unsigned_Long (K));
         pragma Debug (Ada.Text_IO.Put_Line ("Iteration " & M'Image & " for number " & K'Image));
         pragma Debug (Ada.Text_IO.Put_Line ("DEBUG: (function Is_A033919) : N is " & Image (N)));
         if Is_Prime (N) then
            return False;
         end if;
         M := M + 1;
      end loop;
      return True;
   end Is_A033919;

   subtype Odd is Positive
     with Dynamic_Predicate => Odd mod 2 /= 0;

   Sequence_Number : Odd;
   Max_Sequence_Number : constant Positive := 771; -- First integer must be 773
   I : Odd := 3;

begin
   Load_Powers_of_Two (Powers_of_Two);
   pragma Debug (Show_Powers_of_Two (Powers_of_Two));
   loop
      pragma Debug (Ada.Text_IO.Put ("DEBUG: (main program): I is "));
      pragma Debug (Ada.Integer_Text_IO.Put (I, 0));
      pragma Debug (Ada.Text_IO.New_Line);

      if Is_A033919 (I) then
         Sequence_Number := I; --
         Ada.Text_IO.Put ("*****An answer was found!  ");
         Ada.Integer_Text_IO.Put (Sequence_Number, 5);
         Ada.Text_IO.New_Line;
      end if;
      exit when I >= Max_Sequence_Number;
      I := I + 2;
   end loop;
end A033919_GMP;
