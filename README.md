ada-py-bind
===========

Introduction
------------

ada-py-bind is an Ada library, based upon `GNATCOLL.Python`, meant to simplify
the binding of Ada data structures to Python.

The idea is that ada-py-bind will handle a certain number of things for you
automatically or semi-automatically, such as:

- Memory management and ref-counting.
- Type checking of arguments, both dynamic (in Python) and static (enforcing
  correct types for bindings through Ada's type system).
- Propagating Ada exceptions to Python
- Simple getter/setter type checking
- Common conversions from Ada types to Python

Simple example
--------------

Given this Ada package:

```ada
package Shape is
   type Point is record
      X, Y : Integer := 12;
   end record;

   function Get_X (Self : Point) return Integer;
   procedure Set_X (Self : in out Point; X : Integer);
   function Get_Y (Self : Point) return Integer;
   procedure Set_Y (Self : in out Point; Y : Integer);

   type Shape is record
      Id : Unbounded_String;
      Position : Point;
   end record;

   function Get_Id (Self : Shape) return String;
   procedure Set_Id (Self : in out Shape; Id : String);
   procedure Translate (Self : in out Shape; Move : Point);
end Shape;
```

You can create bindings to python with the following binding code


```ada
package Shape.Py_Bind is
   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Point_X_Prop is new Py_Point_Prop.Byval
     (Int_Type, "x", Get_X, Set_X);
   package Py_Shape_Y_Prop is new Py_Point_Prop.Byval
     (Int_Type, "y", Get_Y, Set_Y);

   --  Bind shape
   package Py_Shape is new Py_Value (Shape, Module, "Shape");
   package Py_Shape_Prop is new Py_Property (Py_Shape);

   package Py_Shape_Id_Prop is new Py_Shape_Prop.Byval
     (String_Type, "id", Get_Id, Set_Id);

   function Get_Position (S : Py_Shape.Rec_Access) return Py_Point.Rec_Access;
   procedure Set_Position
     (S : Py_Shape.Rec_Access; Point : Py_Point.Rec_Access);
   package Py_Shape_Position_Prop is new Py_Shape_Prop.Byref
     (Py_Point.Access_Desc, "position", Get_Position, Set_Position);

   function Dummy_Method (Args : Py_Args) return PyObject;
   function Py_Translate (Args : Py_Args) return PyObject;

   package Py_Shape_Dummy_Method is new Py_Bind.Py_Functions.Raw_Method
     ("dummy", Dummy_Method, Py_Shape, Doc => "Dummy doc");

   package Py_Shape_Translate is new Py_Bind.Py_Functions.Raw_Method
     ("translate", Py_Translate, Py_Shape,
      Doc => "Dummy doc",
      Profile => Create_Profile ((Arg_Spec ("self", Py_Shape.Py_Type),
                                  Arg_Spec ("move", Py_Point.Py_Type))));

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
   --  The initialize procedure needs to be named
   --  init{name_of_the_python_module} for the dynamic library to work from python.
end Shape.Py_Bind;
```

The code of the module body, which contains the body of setters, as well as the
Initialize_Module stub, is omitted here, but you can check a complete example
[here](./testsuite/tests/simple).


Limitations
-----------

ada-py-bind was mildly inspired by
[pybind11](https://github.com/pybind/pybind11). However, several things make it
impossible to make ada-py-bind as expressive:

1. Lack of implicit generic instantiation will force a lot of boilerplate.
2. Lack of [pointer to data members](https://stackoverflow.com/a/670744) will
   force having getters and setters for every data member.
3. Lack of [variadic
   generics](https://en.cppreference.com/w/cpp/language/parameter_pack), along
   with the fact that an Ada formal parameter has a lot more attributes than
   just the type, that are not stored in the type (mode, constness, aliasing,
   etc), make it impossible to create a generic, or even a group of generics,
   that will automatically bind an Ada function to a Python one.
