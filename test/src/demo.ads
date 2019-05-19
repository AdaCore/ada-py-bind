with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   pragma Suppress (Access_Check);

   type Pet is record
      Name : Unbounded_String;
   end record;

   function Get_Name (Self : Pet) return String is
     (To_String (Self.Name));

   procedure Set_Name (Self : in out Pet; Name : String);

   ---------------------
   -- Python bindings --
   ---------------------

   package Pet_Module is new Py_Module ("test");
   package Py_Pet is new Py_Value (Pet, Pet_Module.Desc, "Pet");
   package Py_Pet_Name_Prop is new Py_Property
     (Py_Pet, String_Type, "name", Get_Name, Set_Name);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "inittest");
end Demo;
