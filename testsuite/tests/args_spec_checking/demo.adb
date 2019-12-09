with Ada.Text_IO; use Ada.Text_IO;

package body Demo is

   --------------
   -- Get_Name --
   --------------

   function Get_Id (Self : Shape) return String is
   begin
      return To_String (Self.Id);
   end Get_Id;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Id (Self : in out Shape; Id : String) is
   begin
      Self.Id := To_Unbounded_String (Id);
   end Set_Id;

   -----------
   -- Get_X --
   -----------

   function Get_X (Self : Point) return Integer is (Self.X);

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Self : in out Point; X : Integer) is
   begin
      Self.X := X;
   end Set_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Self : Point) return Integer is (Self.Y);

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Self : in out Point; Y : Integer) is
   begin
      Self.Y := Y;
   end Set_Y;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module is
   begin
      Module.Initialize_Module;
   end Initialize_Module;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Position (S : Py_Shape.Val_Access) return Py_Point.Val_Access
   is
   begin
      return S.Position'Unrestricted_Access;
   end Get_Position;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Position
     (S : Py_Shape.Val_Access; Point : Py_Point.Val_Access)
   is
   begin
      Put_Line (Point.X'Image);
      Put_Line (Point.Y'Image);
      S.Position := Point.all;
   end Set_Position;

   ------------------
   -- Dummy_Method --
   ------------------

   function Dummy_Method (Args : Py_Args) return PyObject is
      pragma Unreferenced (Args);
   begin
      Put_Line ("Hello from Dummy_Method");
      return PyInt_FromLong (1243);
   end Dummy_Method;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Self : in out Shape; Move : Point) is
   begin
      Self.Position.X := Self.Position.X + Move.X;
      Self.Position.Y := Self.Position.Y + Move.Y;
   end Translate;

   ------------------
   -- Py_Translate --
   ------------------

   function Py_Translate (Args : Py_Args) return PyObject is
      Self : constant Py_Shape.Val_Access :=
        Py_Shape.Access_Desc.Get_Arg (Args, 1);
      P    : constant Point := Py_Point.Val_Desc.Get_Arg (Args, 2);
   begin
      Translate (Self.all, P);
      return Py_None;
   end Py_Translate;

end Demo;
