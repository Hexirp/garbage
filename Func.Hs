module Func where
    data Program = Program Module BigBlank Program
    data Module = Module ModuleT Blank ModuleName Blank WhereT EOL Code
    data Code = Code Indent MiniCOde EOL Code
    data MiniCode = MDef | TDef | FDef