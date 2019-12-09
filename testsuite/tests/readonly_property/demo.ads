with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Python; use GNATCOLL.Python;

with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Py_Functions;

package Demo is
   type Point is record
      X, Y : Integer := 12;
   end record;

   function Get_X (Self : Point) return Integer;
   function Get_Y (Self : Point) return Integer;

   ---------------------
   -- Python bindings --
   ---------------------

   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Point_X_Prop is new Py_Point_Prop.Read_Only
     (Int_Type, "x", Get_X);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;
