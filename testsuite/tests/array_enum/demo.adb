package body Demo is
   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module is
   begin
      Module.Initialize_Module;
   end Initialize_Module;

   ----------------
   -- Get_Coords --
   ----------------

   function Get_Coords
     (P : Py_Point.Val_Access) return Py_Coords.Py_Container.Val_Access
   is
   begin
      return P.Coords'Unrestricted_Access;
   end Get_Coords;

   ----------------
   -- Set_Coords --
   ----------------

   procedure Set_Coords
     (P : Py_Point.Val_Access; Val : Py_Coords.Py_Container.Val_Access) is
   begin
      P.Coords := Val.all;
   end Set_Coords;

end Demo;
