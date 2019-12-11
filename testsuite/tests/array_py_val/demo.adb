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

   ----------------
   -- Get_Points --
   ----------------

   function Get_Points
     (P : Py_Plot.Val_Access) return Py_Point_Array.Py_Container.Val_Access
   is
   begin
      return P.Points'Unrestricted_Access;
   end Get_Points;

   ----------------
   -- Set_Points --
   ----------------

   procedure Set_Points
     (P : Py_Plot.Val_Access; Val : Py_Point_Array.Py_Container.Val_Access)
   is
   begin
      P.Points := Val.all;
   end Set_Points;

end Demo;
