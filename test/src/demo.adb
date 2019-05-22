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
      Test_Module.Initialize_Module;
   end Initialize_Module;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Position (S : Py_Shape.Rec_Access) return Py_Point.Rec_Access
   is
   begin
      return S.Position'Unrestricted_Access;
   end Get_Position;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Position
     (S : Py_Shape.Rec_Access; Point : Py_Point.Rec_Access)
   is
   begin
      S.Position := Point.all;
   end Set_Position;

end Demo;
