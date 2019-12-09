package body Demo is

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

end Demo;
