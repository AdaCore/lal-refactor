# LAL Refactor

This repository provides a collection of source code refactoring tools for the Ada programming language, leveraging the power of [Libadalang](https://github.com/AdaCore/libadalang). These tools aim to automate and assist with common code refactoring tasks, improving code maintainability and reducing the risk of introducing errors during the refactoring process.

Currently, the main user of these tools is the [Ada Language Server](https://github.com/AdaCore/ada_language_server).

## Implemented Tools

### Safe Rename

The Safe Rename tool not only allows you to safely rename Ada entities such as variables, constants, types, packages, and subprograms throughout your codebase but also includes additional features to ensure the integrity of your code during the renaming process. Currently, it is able to: detect potential name collisions that may occur when renaming an entity; detect instances where the renamed entity might become hidden for another entity, causing the runtime behaviour of the program to change silently.

### Change Subprogram Signatures

The Change Subprogram Signatures tool provides several sub-tools to modify subprogram signatures in a controlled and automated manner. The following sub-tools are available:

- **Add Parameter**: Easily add a new parameter to a subprogram. Currently, all call sites need to be manually handled.
- **Remove Parameter**: Safely remove a parameter from a subprogram, handling the necessary adjustments to all references.
- **Move Parameter**: Move a parameter from one position to another within a subprogram, automatically updating call sites (if necessary).
- **Change Parameter Mode**: Change the mode (in, out, in out) of a parameter in a subprogram, ensuring consistency across the codebase.
- **Change Parameter Default Value**: Modify the default value of a parameter in a subprogram.
- **Change Parameter Type**: Update the type of a parameter in a subprogram.
- **Change Function Return Type**: Change the return type of a function. Currently, all calls and assignments need to be handled manually.

### Pull-Up Declaration

The Pull-Up Declaration tool assists you in refactoring code by moving a declaration from a child unit to its parent unit. It simplifies the code structure, avoids duplication, and promotes better organization of your Ada code. In addition, the tool ensures that dependent declarations are also moved when pulling up a declaration.

#### Handling Dependent Declarations

When you use the Pull-Up Declaration tool to move a declaration (let's say, declaration A) from a child unit to its parent unit, the tool takes into account any dependent declarations (e.g., declaration B) that A relies on within the same scope. The tool intelligently identifies these dependent declarations and automatically moves them along with the original declaration.

### Replace Type

The Replace Type tool allows you to easily replace a specific type with another type throughout your codebase. This is useful when you want to update a type definition and ensure that all instances of the old type are correctly modified.

### Introduce Parameter

The Introduce Parameter tool enables you to introduce a new parameter to a subprogram based on one of its declarations, automatically modifying the subprogram's signature. This is useful when you need to pass additional data to a subprogram instead of making a declaration inside it.

### Suggest Import

The Suggest Import tool assists you in managing imports by automatically suggesting the necessary import statements for Ada packages that are used but not yet imported.

### Sort Dependencies

The Sort Dependencies tool provides functionality to sort the with and use clauses of an Ada source files, ensuring a consistent and organized order of package imports.

### Suppress Separate Subprogram

The Suppress Separate Subprogram tool allows you to suppress a separate subprogram by removing its declaration and merging its body with the parent unit. This helps simplify code organization and improve readability.

### Extract Variable

The Extract Variable tool identifies expressions which can be simplified to a variable, and moves the relevant code for you. This helps improve readability and abstraction.

### Extract Subprogram

The Extract Subprogram tool identifies blocks of expressions or statements which can be extracted into a separate procedure or function (detected from context), and generates the relevant declaration and implementation, replacing the code with a subprogram call. This can improve code readability and complexity.

### Generate Package Body

If a package spec declares subprograms, two sub-tools are available:

- **Generate Package Body**: if no matching package body found, create a new body file with subprogram body stubs fromt he spec.
- **Update Package Body** can either generate a new package body file and fill it, or if a matching package body already exists, update this with subprogram stubs for new declarations. This aims to reduce developer time spent writing boilerplate code and speed up implementations.

### Generate Subprogram Body

This tool identifies nested subprogram declarations without a matching body in the file, and generates a subprogram body stub for the user to fill. This reduces developer time spent writing boilerplate code.

## Contributing

Contributions to this project are welcome! If you encounter any issues or have ideas for additional refactoring tools, feel free to open an issue or submit a pull request.
