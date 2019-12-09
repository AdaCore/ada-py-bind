with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Bounded_Array;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   type Coords_Type is array (Integer range 1 .. 3) of Integer;

   type Point is record
      Coords : Coords_Type := (0, 0, 0);
   end record;

   ---------------------
   -- Python bindings --
   ---------------------

   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   subtype Coords_Index is Integer range Coords_Type'Range;
   package Py_Coords_Index is new Simple_Discrete_Binding (Coords_Index);

   package Py_Coords is new Py_Bind.Bounded_Array
     (Coords_Index, Py_Coords_Index.Type_Desc, Integer,
      Int_Type, Coords_Type, Module, "Coords");

   function Get_Coords
     (P : Py_Point.Val_Access) return Py_Coords.Py_Array_Value.Val_Access;

   procedure Set_Coords
     (P : Py_Point.Val_Access; Val : Py_Coords.Py_Array_Value.Val_Access);

   package Py_Point_Coords_Prop is new Py_Point_Prop.Byref
     (Py_Coords.Py_Array_Value.Access_Desc, "coords", Get_Coords, Set_Coords);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;
