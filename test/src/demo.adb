package body Demo is
   procedure Set_Name (Self : in out Pet; Name : String) is
   begin
      Self.Name := To_Unbounded_String (Name);
   end Set_Name;

   procedure Initialize_Module is
   begin
       Pet_Module.Initialize_Module;
   end Initialize_Module;
end Demo;
